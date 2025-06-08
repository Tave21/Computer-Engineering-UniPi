package it.unipi.dii.xavier

import android.view.LayoutInflater
import android.view.View
import android.view.ViewGroup
import android.widget.TextView
import androidx.recyclerview.widget.RecyclerView

data class Device(
    var name: String,
    var model: String,
    var brand: String,
    var x: Double,
    var y: Double,
    var fps: Double
)

class DeviceAdapter(
    private var devices: List<Device>,
    private val onItemClick: (Device) -> Unit
) : RecyclerView.Adapter<DeviceAdapter.DeviceViewHolder>() {

    inner class DeviceViewHolder(itemView: View) : RecyclerView.ViewHolder(itemView) {
        val textView: TextView = itemView.findViewById(R.id.deviceText)

        fun bind(device: Device) {
            textView.text = "${device.brand} ${device.name} ${device.model}"
            itemView.setOnClickListener {
                onItemClick(device)
            }
        }
    }

    override fun onCreateViewHolder(parent: ViewGroup, viewType: Int): DeviceViewHolder {
        val view = LayoutInflater.from(parent.context)
            .inflate(R.layout.device_item_list, parent, false)
        return DeviceViewHolder(view)
    }

    override fun onBindViewHolder(holder: DeviceViewHolder, position: Int) {
        holder.bind(devices[position])
    }

    override fun getItemCount(): Int = devices.size

    fun updateList(newDevices: List<Device>) {
        this.devices = newDevices
        notifyDataSetChanged()
    }
}
