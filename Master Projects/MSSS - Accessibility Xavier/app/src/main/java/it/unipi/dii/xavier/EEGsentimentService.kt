package it.unipi.dii.xavier

import android.app.NotificationChannel
import android.app.NotificationManager
import android.net.ConnectivityManager
import android.net.NetworkCapabilities
import android.util.Log
import java.nio.ByteBuffer
import java.nio.ByteOrder
import com.chaquo.python.Python
import mylibrary.mindrove.Instruction
import mylibrary.mindrove.SensorData
import mylibrary.mindrove.ServerManager
import androidx.lifecycle.lifecycleScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.Job
import kotlinx.coroutines.delay
import kotlinx.coroutines.isActive
import kotlinx.coroutines.launch
import android.app.Service
import android.content.Context

import android.content.Intent
import android.os.Build
import android.os.IBinder
import androidx.core.app.NotificationCompat
import camp.visual.eyedid.gazetracker.GazeTracker
import kotlinx.coroutines.*
import java.util.LinkedList


class EEGsentimentService : Service() {
    private lateinit var serverManager: ServerManager  //gestore dei dati di ingresso del EEG

    //Gaze tracker responsible for capturing gaze and blink data
    private var gazeTracker: GazeTracker? = GazeTrackerSingleton.tracker

    //STATI APPLICAZIONE
    private var isServerManagerStarted = false
    private var isEEGConnected = false
    private var mentalStatus="No mental status yet"

    //Variabili per il filtro dello stato mentale
    private val mentalStatusHistory = LinkedList<Boolean>()
    private val mentalStatusWindowSize = 5 // Dimensione della finestra per la stabilizzazione
    private val mentalStatusStabilityThresholdPercentage = 0.6 // Soglia di maggioranza


    //BUFFER
    private val EEG_fs=500 //frequenza campionamento EEG
    private val canali_parsati=6 //numero di canali parsati a python (EEG ne ha 6 utilizzabili)
    private val window_size=2    //grandezza della finestra di dati in secondi
    private val campioni_buffer_totali=EEG_fs*canali_parsati*window_size
    private val python_timer :Long =500  //frequenza di lancio di python in ms
    private val python_window=1  //grandezza finestra di python in secondi
    private  val campioni_buffer_python=EEG_fs*python_window*canali_parsati
    private val eegBuffer = CircularEEGBuffer(campioni_buffer_totali,campioni_buffer_python)

    //Riferimento al codice python
    private val py = Python.getInstance()
    private val pyModule = py.getModule("XavierClassifier") //file python associato in app/src/main/python/...
    private val funInitModel="model_init"  //nome della funzione nel file python
    private val funClassifier="EEG_classifier"
    private var isPythonBusy = false  //impedisce che più funzioni python vengano lanciate contemporaneamente

    private var serviceScope = CoroutineScope(Dispatchers.IO + SupervisorJob())
    private var serviceInitScope = CoroutineScope(Dispatchers.IO + SupervisorJob())
    private var modelInitJob: Job? = null
    private var isModelInitialized = false

    override fun onStartCommand(intent: Intent?, flags: Int, startId: Int): Int {
        if (!isNetworkAvailable()) {
            Log.e("AppProcess", "No network connection. Please enable Wi-Fi.")
            stopSelf()
            return START_NOT_STICKY
        }

        createNotificationChannel(this)
        val notification = NotificationCompat.Builder(this, "eeg_channel_id")
            .setContentTitle("EEG attivo")
            .setContentText("Inizializzazione modello")
            .setSmallIcon(R.drawable.ic_brain)
            .build()
        startForeground(1, notification)

        Log.i("AppProcess","Initializing XavierModel in python (non-blocking)")

        modelInitJob = serviceInitScope.launch {
            Log.i("AppProcess","Launching model initialization...")
            val result = withContext(Dispatchers.Default) { pyModule.callAttr(funInitModel).toBoolean() }
            Log.i("AppProcess","Model initialization returned: $result")
            if (result){
                Log.i("AppProcess", "Model initialized successfully")
                isModelInitialized = true
                // Avvia il server manager solo dopo che il modello è inizializzato
                startServerManager()
            } else {
                Log.e("AppProcess", "Model initialization failed")
                stopSelf() // O gestisci l'errore in modo appropriato
            }
        }

        // Il server manager NON viene avviato qui direttamente.
        // Verrà avviato solo dopo il completamento di modelInitJob.
        return START_STICKY
    }

    private fun startServerManager() {
        Log.i("AppProcess","Starting server manager")
        try {
            //Funzione che gestisce i dati quando arrivano
            serverManager = ServerManager { sensorData: SensorData ->
                //SCRITTURA DATI SUL BUFFER
                eegBuffer.addSamples(//conversione double ->float (non serve tutta questa precisione)
                    listOf(
                        sensorData.channel1,
                        sensorData.channel2,
                        sensorData.channel3,
                        sensorData.channel4,
                        sensorData.channel5,
                        sensorData.channel6
                    )
                )
                Log.d("EEGsignal", "Received channel1: ${sensorData.channel1}")
                Log.d("EEGsignal", "Approximation: ${sensorData.channel1.toFloat()}")

                if (!isEEGConnected && isModelInitialized) {
                    isEEGConnected = true
                    //attiva il codice python per analizzare i dati
                    startStreaming()  // Avvia il timer SOLO dopo il primo campione ricevuto E il modello è pronto
                    Log.i("AppProcess","Streaming started")
                }
            }

            //serverManager.sendInstruction(Instruction.EEG)

            Log.i("AppProcess", "Instruction EEG inviata")
            serverManager.start() //cerca di avviare il gestore del EEG
            Log.i("AppProcess","Server manager started")
        }catch (e: Exception){
            Log.e("AppProcess", "Error starting server manager: ${e.message}")
            //chiude l'applicazione in caso di errore
            stopSelf()
        }
        isServerManagerStarted = true
    }

    private fun updateMentalStatus(prediction: Boolean) {
        mentalStatusHistory.addLast(prediction)
        if (mentalStatusHistory.size > mentalStatusWindowSize) {
            mentalStatusHistory.removeFirst()
        }

        if (mentalStatusHistory.size >= mentalStatusWindowSize) {
            val trueCount = mentalStatusHistory.count { it }
            val truePercentage = trueCount.toDouble() / mentalStatusHistory.size
            mentalStatus = if (truePercentage >= mentalStatusStabilityThresholdPercentage) "Stabile (True)" else "Stabile (False)"

            if (GazeTrackerSingleton.isInitialized) {
                if(mentalStatus == "Stabile (False)"){
                    gazeTracker?.let {
                        if (!it.isTracking) {
                            it.startTracking()
                        }
                    }
                } else {
                    gazeTracker?.let {
                        if (it.isTracking == true) {
                            it.stopTracking()
                        }
                    }
                }
            } else {
                Log.d("AppProcess", "Eye tracker not initialized.")
            }

        } else {
            // Se non abbiamo abbastanza dati per la finestra, mostriamo lo stato grezzo
            mentalStatus = if (prediction) "Stabile (True)" else "Instabile (False)"
        }

    }

    //Metodo che gestisce il timer per lanciare il codice python
    private fun startStreaming() {
        serviceScope.launch(Dispatchers.IO) {
            while (isActive && isModelInitialized) { // Verifica che il modello sia inizializzato
                try {
                    if (eegBuffer.hasEnoughData() && !isPythonBusy) {
                        isPythonBusy = true
                        val predizione =  withContext(Dispatchers.Default) { //siccome è CPU-intensive
                            pyModule.callAttr(
                                funClassifier,
                                eegBuffer.getFloatArray(),
                                canali_parsati
                            ).toInt()
                        }


                        if (predizione==0)
                            updateMentalStatus(true) //occhi aperti
                        else
                            updateMentalStatus(false) //occhi chiusi


                        Log.e("AppProcess", "Intent broadcasted: $predizione")
                        Log.i("AppProcess", "Mental status: $mentalStatus")

                    }
                } catch (e: Exception) {
                    Log.e("PyBridge", "Python error: ${e.message}")
                } finally {
                    isPythonBusy = false
                }
                delay(python_timer)
            }
        }
    }

    fun createNotificationChannel(context: Context) {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            val channel = NotificationChannel(
                "eeg_channel_id",
                "EEG Monitoring",
                NotificationManager.IMPORTANCE_LOW // basso rumore
            )
            channel.description = "Canale per il monitoraggio EEG in tempo reale"
            val notificationManager = context.getSystemService(NotificationManager::class.java)
            notificationManager.createNotificationChannel(channel)
        }
    }

    override fun onDestroy() {
        super.onDestroy()
        modelInitJob?.cancel()
        serviceScope.cancel()
        //Ferma il ServerManager.
        if (isServerManagerStarted){
            serverManager.stop()
        }
    }
    override fun onBind(intent: Intent?): IBinder? = null
    private fun isNetworkAvailable(): Boolean {
        val connectivityManager = getSystemService(CONNECTIVITY_SERVICE) as ConnectivityManager
        val network = connectivityManager.activeNetwork
        val capabilities = connectivityManager.getNetworkCapabilities(network)
        return capabilities != null &&
                (capabilities.hasTransport(NetworkCapabilities.TRANSPORT_WIFI) ||
                        capabilities.hasTransport(NetworkCapabilities.TRANSPORT_CELLULAR))
    }
}


class CircularEEGBuffer(private val capacity: Int, private val windowSize : Int) {
    private val grandezza_campione=8   //grandezza di un campione in byte (float64)
    private val buffer: ByteBuffer = ByteBuffer.allocateDirect(capacity * grandezza_campione).order(ByteOrder.nativeOrder())
    private var writePos = 0
    private var readPos = 0

    fun addSample(sample: Double) {
        buffer.putDouble((writePos % capacity) * grandezza_campione, sample)
        writePos = (writePos + 1) % capacity
    }

    fun addSamples(samples: List<Double>) {
        for (sample in samples) {
            addSample(sample)
        }
    }

    fun hasEnoughData(): Boolean {
        val distance = if (writePos >= readPos) writePos - readPos else capacity - (readPos - writePos)
        return distance >= windowSize
    }

    fun getFloatArray(): DoubleArray {
        val output = DoubleArray(windowSize)
        var localRead = readPos
        for (i in 0 until windowSize) {
            output[i] = buffer.getDouble((localRead % capacity) * grandezza_campione)
            localRead = (localRead + 1) % capacity
        }
        readPos = (readPos + windowSize) % capacity
        return output
    }
}