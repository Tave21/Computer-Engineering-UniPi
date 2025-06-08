package it.unipi.dii.xavier

import GazeTrackerSingleton
import android.accessibilityservice.AccessibilityService
import android.accessibilityservice.GestureDescription
import android.annotation.SuppressLint
import kotlin.math.abs
import android.app.NotificationChannel
import android.app.NotificationManager
import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent
import android.content.IntentFilter
import android.content.pm.PackageManager
import android.content.res.Resources
import android.graphics.PixelFormat
import android.graphics.PorterDuff
import android.os.Build
import android.os.Handler
import android.os.Looper
import android.provider.Settings
import android.util.Log
import android.view.Gravity
import android.view.LayoutInflater
import android.view.View
import android.view.ViewConfiguration
import android.view.WindowInsets
import android.view.WindowManager
import android.view.accessibility.AccessibilityEvent
import android.widget.ImageView
import androidx.annotation.RequiresApi
import androidx.core.app.NotificationCompat
import androidx.core.graphics.toColorInt
import androidx.core.view.isVisible
import androidx.lifecycle.lifecycleScope
import camp.visual.eyedid.gazetracker.GazeTracker
import camp.visual.eyedid.gazetracker.callback.TrackingCallback
import camp.visual.eyedid.gazetracker.metrics.BlinkInfo
import camp.visual.eyedid.gazetracker.metrics.FaceInfo
import camp.visual.eyedid.gazetracker.metrics.GazeInfo
import camp.visual.eyedid.gazetracker.metrics.UserStatusInfo
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.Job
import kotlinx.coroutines.cancel
import kotlinx.coroutines.delay
import kotlinx.coroutines.isActive
import kotlinx.coroutines.launch
import kotlinx.coroutines.withContext
import java.lang.Thread.sleep


class GazeTrackerService : AccessibilityService() {

    //manage the overlay view
    private lateinit var windowManager: WindowManager

    //represent the pointer
    private lateinit var pointerView: ImageView

    //parameters to be used when adding the pointer view
    private lateinit var layoutParams: WindowManager.LayoutParams

    //Gaze tracker responsible for capturing gaze and blink data
    private var gazeTracker: GazeTracker? = null

    //BroadcastReceiver to start the gaze tracking when calibration is completed
    private lateinit var startReceiver: BroadcastReceiver

    private var clickModeDS=false //indicate which mode use for blink detection
    // for dwell-click (false)
    private var lastX = 0f
    private var lastY = 0f
    private var dwellStart: Long = 0
    //threshold for dwell-click
    private val dwellTreshold = 1500L // ms

    //for snap blink (true)
    private var blink_counter=0
    private val blinkTreshold = 800L // ms


    //for swipe
    private var currentZone: String? = null
    private var zoneStart: Long = 0L
    private val zoneTreshold = 1000L  // ms to focus for swipe
    private var screenW = 0
    private var screenH = 0

    //navigation bar menu
    private lateinit var navMenu: View
    private lateinit var navMenuParams: WindowManager.LayoutParams

    private lateinit var homePackage: String
    //track if we are in home to avoid unwanted swipe up (in order to click upper applications)
    private var isHomeScreen = false

    //track if the keyboard is open to avoid unwanted swipe down
    private var isKeyboardOpen = false

    //action string used to trigger gaze tracking after calibration
    companion object {
        const val ACTION_START_GAZE = "it.unipi.dii.xavier.START_GAZE"
    }

    //receiver for keyboard visibility tracking
    private val keyboardReceiver = object: BroadcastReceiver() {
        override fun onReceive(ctx: Context, intent: Intent) {
            when (intent.action) {
                CustomKeyboardIME.ACTION_IME_SHOWN   -> isKeyboardOpen = true
                CustomKeyboardIME.ACTION_IME_HIDDEN  -> isKeyboardOpen = false
            }
        }
    }

    /**
    * Initializes the service, overlay components, notification, and broadcast receivers.
    */
    @SuppressLint("ForegroundServiceType", "UnspecifiedRegisterReceiverFlag", "InflateParams")
    @RequiresApi(Build.VERSION_CODES.O)
    override fun onCreate() {
        super.onCreate()
        // retrieves the dimensions of the screen of the device
        val metrics = Resources.getSystem().displayMetrics
        screenW = metrics.widthPixels
        screenH = metrics.heightPixels

        // get preferences from main activity
        val prefs = getSharedPreferences("app_prefs", MODE_PRIVATE)
        clickModeDS= prefs.getBoolean("clickMode", false)

        //used to draw the pointer on the screen, above other apps
        windowManager = getSystemService(WINDOW_SERVICE) as WindowManager

        //dimensions of the new pointer
        val density = resources.displayMetrics.density
        val sizePx = (32 * density + 0.5f).toInt()

        //creates a new ImageView for the pointer
        pointerView = ImageView(this).apply {
            setImageResource(R.drawable.mouse_pointer)

            scaleType = ImageView.ScaleType.FIT_CENTER
            // Apply a neon green color filter (#39FF14) in SRC_ATOP mode
            setColorFilter("#39FF14".toColorInt(), PorterDuff.Mode.SRC_ATOP)
            visibility = View.INVISIBLE
        }

        //creates the parameters that define where and how the pointer should be displayed
        layoutParams = WindowManager.LayoutParams().apply {
            width = sizePx
            height = sizePx
            format = PixelFormat.TRANSLUCENT
            //to draw on apps
            type = WindowManager.LayoutParams.TYPE_ACCESSIBILITY_OVERLAY
            //the pointer doesn't steel the focus to underneath apps
            flags = WindowManager.LayoutParams.FLAG_NOT_FOCUSABLE or WindowManager.LayoutParams.FLAG_NOT_TOUCHABLE
            //the pointer
            gravity = Gravity.TOP or Gravity.START

        }

        if (!Settings.canDrawOverlays(this)) {
            Log.e("GazeTrackerService", "SYSTEM_ALERT_WINDOW permission not granted. Service stop.")
            stopSelf()
            return
        }
        // creates the notification channel
        val channelId = "GazeTrackerChannel"
        val channel = NotificationChannel(channelId, "Gaze Tracker Service", NotificationManager.IMPORTANCE_LOW)
        val manager = getSystemService(Context.NOTIFICATION_SERVICE) as NotificationManager
        manager.createNotificationChannel(channel)

        val notification = NotificationCompat.Builder(this, channelId)
            .setContentTitle("Active Gaze Tracker")
            .setContentText("The service is tracking the gaze")
            .setSmallIcon(R.drawable.ic_launcher_foreground)
            .build()

        startForeground(1, notification)

        //register the receiver that waits for the calibration end
        startReceiver = object: BroadcastReceiver() {
            override fun onReceive(ctx: Context, intent: Intent) {
                if (intent.action == ACTION_START_GAZE) {
                    //add pointer overlay
                    try { windowManager.addView(pointerView, layoutParams) } catch (_: Exception) {}
                    pointerView.visibility = View.VISIBLE

                    // start gaze-tracker
                    gazeTracker = GazeTrackerSingleton.tracker
                    gazeTracker?.setTrackingCallback(trackingCallback)
                    gazeTracker?.startTracking()
                }
            }
        }
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
            registerReceiver(startReceiver, IntentFilter(ACTION_START_GAZE), RECEIVER_EXPORTED)
        } else {
            registerReceiver(startReceiver, IntentFilter(ACTION_START_GAZE))
        }
        val filter = IntentFilter("it.unipi.dii.xavier.BACK")
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
            registerReceiver(backReceiver, filter, RECEIVER_EXPORTED)
        } else {
            registerReceiver(backReceiver, filter)
        }

        val navigationBarHeight = calcNavigationBarHeight()

        //inflate the navigation menu
        navMenu = LayoutInflater.from(this).inflate(R.layout.nav_menu, null)

        // position the menu in the center of the screen
        navMenuParams = WindowManager.LayoutParams().apply {
            width = 700
            height = 250
            format = PixelFormat.TRANSLUCENT
            type = WindowManager.LayoutParams.TYPE_ACCESSIBILITY_OVERLAY
            gravity = Gravity.CENTER
            y = navigationBarHeight + 50
        }

        // check if home package is available
        val intent = Intent(Intent.ACTION_MAIN).addCategory(Intent.CATEGORY_HOME)
        val res  = packageManager.resolveActivity(intent, PackageManager.MATCH_DEFAULT_ONLY)
        if (res != null) {
            homePackage = res.activityInfo.packageName
        }
        // register the keyboard receiver to track keyboard visibility
        val keyboardFilter = IntentFilter().apply {
            addAction( CustomKeyboardIME.ACTION_IME_SHOWN)
            addAction( CustomKeyboardIME.ACTION_IME_HIDDEN)
        }
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
            registerReceiver(keyboardReceiver, keyboardFilter, RECEIVER_EXPORTED)
        } else {
            registerReceiver(keyboardReceiver, keyboardFilter)
        }

    }
    /**
    * BroadcastReceiver to simulate the back button press when receiving a specific intent.
    */
    private val backReceiver = object : BroadcastReceiver() {
        override fun onReceive(context: Context, intent: Intent) {
            if (intent.action == "it.unipi.dii.xavier.BACK") {
                performGlobalAction(GLOBAL_ACTION_BACK)
            }
        }
    }

    /**
    * Calculates the height of the navigation bar.
    */
    private fun calcNavigationBarHeight(): Int {
        return if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
            // get the current window metrics
            val wm = getSystemService(WINDOW_SERVICE) as WindowManager
            val windowMetrics = wm.currentWindowMetrics
            // take the insets of the navigation bars (bottom bar)
            val insets = windowMetrics.windowInsets
                .getInsetsIgnoringVisibility(WindowInsets.Type.navigationBars())
            insets.bottom
        } else {
            val dp = 48
            (dp * resources.displayMetrics.density + 0.5f).toInt()
        }
    }
    /**
    * Shows the navigation menu overlay.
    */
    private fun showNavMenu() {
        Handler(Looper.getMainLooper()).post {
            navMenu.visibility = View.VISIBLE
        }
    }
    /**
    * Hides the navigation menu overlay.
    */
    private fun hideNavMenu() {
        Handler(Looper.getMainLooper()).post {
            navMenu.visibility = View.GONE
        }
    }

    /**
     * counts blink events and set return true if it is the first blink
     */
    private fun blinkFilter():Boolean{
        if (dwellStart == 0L) dwellStart = System.currentTimeMillis()
        else if (System.currentTimeMillis() - dwellStart > blinkTreshold) {
            dwellStart = System.currentTimeMillis()  // reset
            blink_counter=0
            Log.i("BLINK","Blink${blink_counter}")
            return true
        }
        blink_counter+=1
        return false
    }
    /**
    * Callback for receiving gaze and blink tracking data.
    */
    private val trackingCallback = object : TrackingCallback {
        override fun onMetrics(
            timestamp: Long,
            gazeInfo: GazeInfo,
            faceInfo: FaceInfo,
            blinkInfo: BlinkInfo,
            userStatusInfo: UserStatusInfo
        ) {
            // update pointer position
            Handler(Looper.getMainLooper()).post {
                layoutParams.x = gazeInfo.x.toInt()
                layoutParams.y = gazeInfo.y.toInt()
                windowManager.updateViewLayout(pointerView, layoutParams)
            }

            handleZone(gazeInfo.x, gazeInfo.y)
            // only if we are not in an active zone, let dwell-click
            if (!clickModeDS){
                if (currentZone == null || (currentZone == "SWIPE_UP" && isHomeScreen) || (currentZone == "DOWN" && isKeyboardOpen)) {
                    handleDwellClick(gazeInfo.x, gazeInfo.y)
                }
                //open the menu with right blink
                if(blinkInfo.isBlinkRight && !blinkInfo.isBlinkLeft && !navMenu.isVisible){
                    showNavMenu()
                //close the menu with left blink
                }else if(blinkInfo.isBlinkLeft && !blinkInfo.isBlinkRight){
                    hideNavMenu()
                }
            }else{
                if (blinkInfo.isBlinkRight && !blinkInfo.isBlinkLeft){
                    if (blinkFilter()) {
                        Log.i("BLINK", "Filtro oltrepassato")
                        performClick(gazeInfo.x.toInt(), gazeInfo.y.toInt())
                        Log.i("BLINK","Blink effettuato")
                    }
                }else if(!blinkInfo.isBlinkRight && blinkInfo.isBlinkLeft){
                    if (blinkFilter()) {
                        if(!navMenu.isVisible){
                            showNavMenu()
                        }else{
                            hideNavMenu()
                        }
                    }
                }
            }
        }
        override fun onDrop(timestamp: Long) { /*…*/ }
    }

   /**
   * Performs a dwell-click if the pointer remains still for a set duration.
   */

    private fun handleDwellClick(x: Float, y: Float) {
        // check if the pointer stays in a small area for a little period of time
        if (abs(x - lastX) < 20 && abs(y - lastY) < 20) {
            //The condition is true if both the x and y differences are less than 20. This effectively
            // checks if the pointer has stayed within a small 20x20 pixel square around the lastX, lastY position.

            // If dwellStart is 0L (which is likely used as an initial or reset value), it means the dwell timer hasn't started yet. In this case,
            // dwellStart is updated with the current system time in milliseconds using System.currentTimeMillis().
            if (dwellStart == 0L) dwellStart = System.currentTimeMillis()
            // If the calculated duration is greater than dwellTreshold, the condition is true, indicating that the dwell time has been reached.
            else if (System.currentTimeMillis() - dwellStart > dwellTreshold) {
                // perform click in the point x,y
                performClick(x.toInt(), y.toInt())
                dwellStart = 0L  // reset
                //gazeTracker?.stopTracking()
                //sleep(2000)
                //gazeTracker?.startTracking()
            }
        } else {
            dwellStart = 0L
        }
        lastX = x
        lastY = y
    }



    /**
    * Detects which zone (edges) the pointer is in and triggers swipe gestures if focus is maintained.
    */
    private fun handleZone(x: Float, y: Float) {
        // check in which zone the pointer is
        val newZone = when {
            x < screenW * 0.05f   -> "LEFT"
            x > screenW * 0.95f   -> "RIGHT"
            y <= screenH * 0.05f   -> "UP"
            y > screenH * 0.05f && y <= screenH * 0.15f  -> "SWIPE_UP"
            y > screenH * 0.85f && y < screenH * 0.95f  -> "DOWN"
            else                 -> null
        }

        if (newZone == null) {
            // if we are in the central zone: reset
            currentZone = null
            zoneStart = 0L
            return
        }

        if (newZone == currentZone) {
            // you are looking in the same zone
            if (zoneStart == 0L) zoneStart = System.currentTimeMillis()
            else if (System.currentTimeMillis() - zoneStart > zoneTreshold) {
                // passed the threshold, execute the action
                when (newZone) {
                    "LEFT"  -> performSwipeLeft()
                    "RIGHT" -> performSwipeRight()
                    "UP"    -> performGlobalAction(GLOBAL_ACTION_NOTIFICATIONS)
                    "SWIPE_UP" -> {
                        //swipe up if we are inside an application
                        if (!isHomeScreen) {
                            performSwipeUp()
                        }
                    }
                    "DOWN" -> {
                        //swipe down if we the keyboard is not opened
                        if(!isKeyboardOpen){
                            performSwipeDown()
                        }
                    }
                }
                // reset
                zoneStart = 0L
            }
        } else {
            // in a new zone
            currentZone = newZone
            zoneStart = 0L
        }
    }

    /**
    * Simulates a right swipe gesture using accessibility APIs.
    */
    private fun performSwipeRight() {
        val path = android.graphics.Path().apply {
            moveTo(screenW * 0.9f, screenH / 2f)
            lineTo(screenW * 0.1f, screenH / 2f)
        }
        val stroke = GestureDescription.StrokeDescription(path, 0, 500L)
        // perform swipe right gesture
        dispatchGesture(GestureDescription.Builder().addStroke(stroke).build(), null, null)
    }

    /**
    * Simulates a left swipe gesture using accessibility APIs.
    */
    private fun performSwipeLeft() {
        val path = android.graphics.Path().apply {
            moveTo(screenW * 0.1f, screenH / 2f)
            lineTo(screenW * 0.9f, screenH / 2f)
        }
        val stroke = GestureDescription.StrokeDescription(path, 0, 500L)
        // perform swipe left gesture
        dispatchGesture(GestureDescription.Builder().addStroke(stroke).build(), null, null)
    }

    /**
    * Simulates an upward swipe gesture using accessibility APIs.
    */
    private fun performSwipeUp() {
        val path = android.graphics.Path().apply {
            moveTo(screenW / 2f, screenH * 0.3f)
            lineTo(screenW / 2f, screenH * 0.98f)
        }
        val stroke = GestureDescription.StrokeDescription(path, 0, 300L)
        // perform swipe up gesture
        dispatchGesture(GestureDescription.Builder().addStroke(stroke).build(), null, null)
    }

    /**
    * Simulates an upward swipe gesture using accessibility APIs.
    */
    private fun performSwipeDown() {
        val path = android.graphics.Path().apply {
            moveTo(screenW / 2f, screenH * 0.9f)
            lineTo(screenW / 2f, screenH * 0.1f)
        }
        val stroke = GestureDescription.StrokeDescription(path, 0, 500L)
        // perform swipe down gesture
        dispatchGesture(GestureDescription.Builder().addStroke(stroke).build(), null, null)
    }

    /**
    * Simulates a click at the given screen coordinates.
    *
    * @param x The X coordinate of the click.
    * @param y The Y coordinate of the click.
    */
    @SuppressLint("ServiceCast")
    private fun performClick(x: Int, y: Int) {
        if (x < 0 || y < 0 )
            return
        // creates a gesture to simulate a tap, creates the point where to click
        val path = android.graphics.Path().apply { moveTo(x.toFloat(), y.toFloat()) }

        // takes the minimum tap duration from the system (≈100 ms)
        val tapDuration = ViewConfiguration.getTapTimeout().toLong()

        // Creates a StrokeDescription with the tap duration
        val stroke = GestureDescription.StrokeDescription(path, 0, tapDuration)

        val gesture = GestureDescription.Builder().addStroke(stroke).build()

        Handler(Looper.getMainLooper()).post {

            // check if the service is enabled
            Settings.Secure.getString(
                contentResolver,
                Settings.Secure.ENABLED_ACCESSIBILITY_SERVICES
            )
           applicationContext.packageName
            // This is the core function call that dispatches the simulated gesture.
            dispatchGesture(gesture, object : GestureResultCallback() {
            }, null)
        }
    }

    /**
    * Called when the service is successfully connected. Sets up navigation menu behavior.
    */
    override fun onServiceConnected() {
        super.onServiceConnected()
        windowManager.addView(navMenu, navMenuParams)
        navMenu.visibility = View.GONE

        // set click listener on ImageView Icons of the navigation menu
        navMenu.findViewById<ImageView>(R.id.btn_back).setOnClickListener {
            Handler(Looper.getMainLooper()).postDelayed({
               performGlobalAction(GLOBAL_ACTION_BACK)
            }, 100)
            hideNavMenu()
        }
        navMenu.findViewById<ImageView>(R.id.btn_home).setOnClickListener {
            performGlobalAction(GLOBAL_ACTION_HOME)
            hideNavMenu()
        }
        navMenu.findViewById<ImageView>(R.id.btn_recents).setOnClickListener {
            performGlobalAction(GLOBAL_ACTION_RECENTS)
            hideNavMenu()
        }
    }

    /**
    * Handles accessibility events. Tracks current app to detect if home screen is active.
    */
    @SuppressLint("SwitchIntDef")
    override fun onAccessibilityEvent(event: AccessibilityEvent?) {
        if (event == null) return

        when (event.eventType) {
            AccessibilityEvent.TYPE_WINDOW_STATE_CHANGED -> {
            }
            AccessibilityEvent.TYPE_VIEW_CLICKED -> {
            }

        }
        if (event.eventType == AccessibilityEvent.TYPE_WINDOW_STATE_CHANGED) {
            val pkg = event.packageName?.toString()
            isHomeScreen = pkg == homePackage
        }
    }

    /**
    * Stops gaze tracking and removes overlays if the service is interrupted.
    */
    override fun onInterrupt() {
        gazeTracker?.stopTracking()
        try { windowManager.removeView(pointerView) } catch (_: Exception) {}
    }

    /**
    * Cleans up resources and unregisters receivers when the service is destroyed.
    */
    override fun onDestroy() {
        unregisterReceiver(startReceiver)
        unregisterReceiver(backReceiver)
        unregisterReceiver(keyboardReceiver)
        gazeTracker?.stopTracking()
        try { windowManager.removeView(pointerView) } catch (_: Exception) {}
        super.onDestroy()
    }
}