package it.unipi.dii.xavier

import android.os.Bundle
import androidx.appcompat.app.AppCompatActivity

class AccessibilitySettingsActivity : AppCompatActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        // Puoi usare un layout minimale, p.es. un TextView con istruzioni per l’utente
        setContentView(R.layout.activity_accessibility_settings)
    }
}
