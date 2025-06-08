import android.annotation.SuppressLint
import camp.visual.eyedid.gazetracker.GazeTracker
//singleton that represents the tracker in common between the MainActivity and the GazeTrackerService
object GazeTrackerSingleton {
    @SuppressLint("StaticFieldLeak")
    var tracker: GazeTracker? = null
    var isInitialized: Boolean = false
}