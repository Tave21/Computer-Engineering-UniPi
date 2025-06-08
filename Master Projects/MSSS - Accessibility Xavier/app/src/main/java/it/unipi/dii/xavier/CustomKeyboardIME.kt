package it.unipi.dii.xavier

import android.annotation.SuppressLint
import android.content.Intent
import android.graphics.drawable.Drawable
import android.inputmethodservice.InputMethodService
import android.view.View
import android.view.ViewGroup
import android.view.inputmethod.InputConnection
import android.widget.Button
import androidx.core.content.ContextCompat
import android.view.inputmethod.EditorInfo

class CustomKeyboardIME : InputMethodService() {

    private var keyboardView: View? = null
    private lateinit var inputConnection: InputConnection
    private var isCapsOn = false
    private var isEmojiOff = true
    private var defaultShiftBackground: Drawable? = null

    companion object {
        const val ACTION_IME_SHOWN   = "com.example.app.ACTION_IME_SHOWN"
        const val ACTION_IME_HIDDEN  = "com.example.app.ACTION_IME_HIDDEN"
    }

    /**
     * Creates and returns the initial keyboard view when the input method is started.
     * This method inflates the main keyboard layout and initializes button listeners.
     */
    @SuppressLint("InflateParams")
    override fun onCreateInputView(): View {

        keyboardView = layoutInflater.inflate(R.layout.keyboard_start, null)
        //find the caps button
        val shiftButton = keyboardView?.findViewById<Button>(R.id.btn_maiusc)
        defaultShiftBackground = shiftButton?.background

        setupMainKeyboard()
        return keyboardView!!
    }

    /**
     * Called when the input view is about to be shown. Broadcasts an intent to notify that the keyboard is visible.
     */
    override fun onStartInputView(info: EditorInfo?, restarting: Boolean) {
        super.onStartInputView(info, restarting)
        // set the keyboard visible
        sendBroadcast(Intent(ACTION_IME_SHOWN)) // Notify that the IME is shown
    }

    /**
     * Sets up the listeners for the main keyboard layout buttons.
     */
    private fun setupMainKeyboard() {
        inputConnection = currentInputConnection

        // Initialize buttons of the main keyboard menu from the layout
        val abcButton = keyboardView?.findViewById<Button>(R.id.btn_letters_abc)
        val jklButton = keyboardView?.findViewById<Button>(R.id.btn_letters_jkl)
        val stuButton = keyboardView?.findViewById<Button>(R.id.btn_letters_stu)
        val numbersButton = keyboardView?.findViewById<Button>(R.id.btn_123)
        val crButton = keyboardView?.findViewById<Button>(R.id.btn_cr)
        val delButton = keyboardView?.findViewById<Button>(R.id.btn_cancel)
        val symbolsButton = keyboardView?.findViewById<Button>(R.id.btn_symbols)
        val openparButton = keyboardView?.findViewById<Button>(R.id.btn_par_open)
        val closeparButton = keyboardView?.findViewById<Button>(R.id.btn_par_close)

        // set the listeners for the layout switching
        abcButton?.setOnClickListener { showKeyboardLayout(R.layout.keyboard_letters_abc) }
        jklButton?.setOnClickListener { showKeyboardLayout(R.layout.keyboard_letters_jkl) }
        stuButton?.setOnClickListener { showKeyboardLayout(R.layout.keyboard_letters_stu) }
        numbersButton?.setOnClickListener { showKeyboardLayout(R.layout.keyboard_numbers) }
        // Enter key
        crButton?.setOnClickListener { inputConnection.commitText("\n", 1) }

        delButton?.setOnClickListener {
            val before = inputConnection.getTextBeforeCursor(2, 0)?.toString() ?: ""
            if (before.length >= 2 && Character.isSurrogatePair(before[0], before[1])) {
                // if the cancel button is clicked and the character to delete is an emoji of two chars
                //delete two chars
                inputConnection.deleteSurroundingText(2, 0)
            } else {
                // if the cancel button is clicked and the character to delete is a simple ascii char
                //delete one char
                inputConnection.deleteSurroundingText(1, 0)
            }
        }
        // Symbols and parentheses buttons
        symbolsButton?.setOnClickListener { showKeyboardLayout(R.layout.keyboard_symbols) }
        openparButton?.setOnClickListener { inputConnection.commitText("(", 1) }
        closeparButton?.setOnClickListener { inputConnection.commitText(")", 1) }
    }

    /**
     * Replaces the current keyboard layout with the specified layout resource.
     * @param layoutId the resource ID of the layout to inflate
     */
    private fun showKeyboardLayout(layoutId: Int) {
        keyboardView = layoutInflater.inflate(layoutId, null)
        setInputView(keyboardView)
        inputConnection = currentInputConnection
        setupKeyboard()
    }

    /**
     * Recursively sets up listeners for all buttons in the current keyboard layout.
     * Handles emoji switching and letter input with caps support.
     */
    private fun setupKeyboard() {
        // flow recursively all the i View (included the LinearLayout and Button) inside the keyboard
        @SuppressLint("InflateParams")
        fun setupButtonListeners(view: View?) {
            if (view is ViewGroup) {
                for (i in 0 until view.childCount) {
                    setupButtonListeners(view.getChildAt(i))
                }
            } else if (view is Button) {
                val buttonText = view.text?.toString()
                if (!buttonText.isNullOrEmpty()) {
                    view.setOnClickListener {
                        // Handle emoji menu switch
                        if(buttonText == "\uD83D\uDE0A" && isEmojiOff){
                            isEmojiOff = false
                            keyboardView = layoutInflater.inflate(R.layout.keyboard_emojis, null)
                            setInputView(keyboardView)
                            setupKeyboard()
                        }else {
                            // Handle caps button selection
                            val letter =
                                if (isCapsOn) buttonText.uppercase() else buttonText.lowercase()
                            inputConnection.commitText(letter, 1)
                        }
                    }
                }
            }
        }

        setupButtonListeners(keyboardView)
        setCommonListeners()
    }

    /**
     * Sets listeners for space, back, and caps buttons (they are in common with some menus).
     * Handles caps lock toggle and restores default caps button appearance.
     */
    private fun setCommonListeners() {
        // Space button
        keyboardView?.findViewById<Button>(R.id.btn_space)?.setOnClickListener {
            inputConnection.commitText(" ", 1)
        }
        // Back button (return to main keyboard)
        keyboardView?.findViewById<Button>(R.id.btn_back)?.setOnClickListener {
            isEmojiOff = true
            showKeyboardLayout(R.layout.keyboard_start)
            setupMainKeyboard()
        }
        // Shift button (caps toggle)
        val shiftButton = keyboardView?.findViewById<Button>(R.id.btn_maiusc)

        shiftButton?.setOnClickListener {
            isCapsOn = !isCapsOn
            updateKeyTexts()
            if (isCapsOn) {
                shiftButton.setBackgroundColor(ContextCompat.getColor(this, R.color.shiftActiveBackground))
            } else {
                shiftButton.background = defaultShiftBackground

            }
        }
    }

    /**
     * Updates all letter buttons in the keyboard layout to reflect current caps lock state.
     */
    private fun updateKeyTexts() {
        fun updateButtons(view: View?) {
            if (view is ViewGroup) {
                for (i in 0 until view.childCount) {
                    updateButtons(view.getChildAt(i))
                }
            } else if (view is Button) {
                val text = view.text?.toString() ?: return
                if (text.length == 1 && text[0].isLetter()) {
                    view.text = if (isCapsOn) text.uppercase() else text.lowercase()
                }
            }
        }
        updateButtons(keyboardView)
    }

    /**
     * Called when the input view is about to be finished. Broadcasts an intent to notify that the keyboard is hidden.
     */
    override fun onFinishInputView(finishingInput: Boolean) {
        super.onFinishInputView(finishingInput)
       //Notify that teh IME is hidden
        sendBroadcast(Intent(ACTION_IME_HIDDEN))
    }
}