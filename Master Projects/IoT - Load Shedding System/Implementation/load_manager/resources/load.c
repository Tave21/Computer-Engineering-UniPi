#include "contiki.h"
#include "coap-engine.h"
#include "sys/log.h"
#include <stdio.h>
// emlearn generated model
#include "eml_net.h"
#include "../Machine-Learning/load_prediction.h"
//Utility functions
#include "../Utility/Timestamp/Timestamp.h"
#include "../Utility/RandomNumberGenerator/RandomNumberGenerator.h"
#include "../Utility/JSONSenML/JSONSenML.h"

//----------------------------------PARAMETERS----------------------------------//

//[+] LOG CONFIGURATION
#define LOG_MODULE "App"
#define LOG_LEVEL LOG_LEVEL_INFO

//[+] POWER PARAMETERS
#define UNIT "kW" // Unit of Power (Load)
// Simulation parameters
#define MAX_LOAD 15 // Maximum load in W
#define MIN_LOAD 0 // Minimum load in W
#define MAX_NIGHT_LOAD 10
#define DAY_STEP 0.025// Load increase simulation step during the day
#define NIGHT_STEP 0.018

//[+] PREDICTION PARAMETER
//Actual date and time in [Year, Month, Day, Hour, Minute] format
extern Timestamp timestamp;

//Correction factor for the prediction
static float correction_factor = 0;
//Scaler parameters for standardizing the input features
static const float SCALER_MEAN[] = {6.5421474, 15.64240547, 11.50424721, 29.51425121}; 
static const float SCALER_SCALE[] = {3.45398155, 8.76187095, 6.9239814, 17.30474549};
// Prediction horizon in minutes
extern int m_sampling_period;

//----------------------------------RESOURCES----------------------------------//
// Load produced by the smart grid in [m_sampling_period] in the future
static float predicted_load = -1;
// Load produced by the smart grid in the last sampling period
static float sampled_load = -1;

//----------------------------FUNCTIONS DEFINITIONS----------------------------------//

static float fake_load_sensing(float old_value);
static void scaler(float* features, float* scaled_features);
static float predict_load();
static void res_get_handler(coap_message_t *request, coap_message_t *response,
                          uint8_t *buffer, uint16_t preferred_size, int32_t *offset);
static void res_event_handler(void);

// Resource definition
EVENT_RESOURCE(
        res_load, 
        "title=\"load\";rt=\"json+senml\";if=\"sensor\";obs", 
        res_get_handler, 
        NULL,
        NULL,
        NULL,
        res_event_handler);

//----------------------------FUNCTIONS DECLARATIONS----------------------------------//

// This function simulates the load sensing
static float fake_load_sensing(float old_value)
{
  // If the old value is -1, it means that the load is not sampled yet
  if (old_value == -1)
    // Average value
    old_value = 7.5;

  // If is night, the load is really low (it is assumed to increase slowly)
  if (timestamp.hour >= 20 || timestamp.hour <= 6)
    return generate_random_float(old_value, MAX_NIGHT_LOAD, MIN_LOAD, NIGHT_STEP, 1);

  // After 8am, the load increases
  if (timestamp.hour >= 6 && timestamp.hour <= 13)
    return generate_random_float(old_value, MAX_LOAD, MIN_LOAD, DAY_STEP, 1);
  
  // Afternoon, the load decrease
  if (timestamp.hour >= 13 && timestamp.hour <= 20)
    return generate_random_float(old_value, MAX_LOAD, MIN_LOAD, DAY_STEP, 0);
  
  LOG_ERR("[Load-manager] Error in the load_sensing function\n");
  
  return 0;
}

// It is needed for standardizing the features in input to the model
static void scaler(float* features, float* scaled_features)
{
  for (int i = 0; i < 4; i++)
  {
    scaled_features[i] = (features[i] - SCALER_MEAN[i]) / SCALER_SCALE[i];
  }
}

// This function predicts the load using the model (without the correction factor)
static float predict(float* features)
{
  float scaled_features[] = {0, 0, 0, 0};
  // Standardize the features
  scaler(features, scaled_features);
  return load_prediction_predict(scaled_features, 4);
}

// This function predicts the load using the previous function (with the correction factor)
static float predict_load()
{
    Timestamp ts_copy = {
      .year = timestamp.year,
      .month = timestamp.month,
      .day = timestamp.day,
      .hour = timestamp.hour,
      .minute = timestamp.minute
    };
    float features[] = {0, 0, 0, 0};
    float prediction = 0;
    // Prints pointer variable 
    printf("%p\n", eml_net_activation_function_strs); // This is needed to avoid compiler error (warnings == errors)

    // Initialize the features
    convert_to_feature(&ts_copy, features);
    // Compute the prediction of the last sampled load: using the current timestamp
    prediction = predict(features);
    // Calibrate the correction factor 
    correction_factor = (sampled_load - prediction) * 0.12 + correction_factor * 0.88;

    // Compute the prediction of the future load computing new timestamp
    advance_time(&ts_copy, m_sampling_period);
    convert_to_feature(&ts_copy, features);
    prediction = predict(features);

    // Apply the correction factor based on the last sampled load and the prediction
    prediction += correction_factor;

    // Check if the prediction is negative
    if (prediction < 0)
      prediction = 0;

    //log information
    LOG_INFO("[Load-manager] Correction Factor: %f\n", correction_factor);
    LOG_INFO("[Load-manager] Predicted Load: %f\n", prediction);   

    return prediction;
}


// Define the resource handler function
static void res_get_handler(coap_message_t *request, coap_message_t *response,
                          uint8_t *buffer, uint16_t preferred_size, int32_t *offset){
    
    LOG_INFO("[Load-manager]//--------------NEW-GET-LOAD-REQUEST---------------------\n");
   
    MeasurementData data[2];
    json_senml js_senml;

    // Future timestamp
    Timestamp future_ts = {
      .year = timestamp.year,
      .month = timestamp.month,
      .day = timestamp.day,
      .hour = timestamp.hour,
      .minute = timestamp.minute
    };
    char names[2][MAX_STR_LEN] = {"predicted", "sampled"};
    char timestamp_str[2][TIMESTAMP_STRING_LEN];
    char base_name[BASE_NAME_LEN];
    int payload_len = 0;
        
    // Timestamp for the future prediction = current timestamp + m_sampling_period
    advance_time(&future_ts, m_sampling_period);
    // Save it in timestamp_str array as first element
    timestamp_to_string(&future_ts, timestamp_str[0]);

    //Timestamp for the current sample, save it in timestamp_str array as second element
    timestamp_to_string(&timestamp, timestamp_str[1]);

    // Create the base name
    get_base_name(base_name);
     
    // Initialize the values of the resources
    // Predicted load
    data[0].name = names[0];
    data[0].unit = UNIT;
    data[0].time = timestamp_str[0];
    data[0].v.v = predicted_load;
    data[0].type = V_FLOAT;

    // Sampled load
    data[1].name = names[1];
    data[1].unit = UNIT;
    data[1].time = timestamp_str[1];
    data[1].v.v = sampled_load;
    data[1].type = V_FLOAT;

    // Create the JSON SenML
    js_senml.base_name = base_name;
    js_senml.base_unit = UNIT;
    js_senml.measurement_data = data;
    js_senml.num_measurements = 2;

    // Convert the JSON SenML to a payload and return the length
    payload_len = json_to_payload(&js_senml, (char*)buffer);
   
    // Check if the payload is valid
    if (payload_len < 0)
    {
      LOG_ERR("\t Error in the json_to_payload function\n");
      coap_set_status_code(response, INTERNAL_SERVER_ERROR_5_00);
      coap_set_payload(response, buffer, 0);
      return;

    } else if (payload_len > preferred_size) // Check if the buffer is big enough
    {
      LOG_ERR("\t Buffer overflow\n");
      coap_set_status_code(response, INTERNAL_SERVER_ERROR_5_00);
      coap_set_payload(response, buffer, 0);
      return;
    }

    // Prepare the response
    // Set the Content-Format header field to "application/json" to be interpreted as a JSON payload
    coap_set_header_content_format(response, APPLICATION_JSON);
    // Set the ETag header field to the length of the payload (used as content version) --> 1 byte ETag lenght
    coap_set_header_etag(response, (uint8_t *)&payload_len, 1);
    // Set the payload to the response
    coap_set_payload(response, buffer, payload_len);

    // Print sended data for debug
    LOG_INFO("[Load-manager] Sending data: %s with size: %d\n", buffer, payload_len);
}

static void res_event_handler(void) {
    char timestamp_str[TIMESTAMP_STRING_LEN];
    timestamp_to_string(&timestamp, timestamp_str);
    LOG_INFO("[Load-manager] -------------------NEW-EVENT--------------\n");
    LOG_INFO("[Load-manager] New sample at time %s\n", timestamp_str);
    // Sample the load
    LOG_INFO("[Load-manager] - Old Sampled Load: %f\n", sampled_load);
    sampled_load = fake_load_sensing(sampled_load);
    LOG_INFO("[Load-manager] - Sampled Load: %f\n", sampled_load);
    // Predict load 
    predicted_load = predict_load();
    // To avoid float convertion problems in DR_manager
    if (predicted_load <= 1.0)
    {
        LOG_ERR("[Load-manager] - Predicted load is less than 1, setting it to 1.1\n");
        predicted_load = 1.1;
    }
    // Notify the observers
    LOG_INFO("[Load-manager] - Notifing the observers\n");
    coap_notify_observers(&res_load);
}
