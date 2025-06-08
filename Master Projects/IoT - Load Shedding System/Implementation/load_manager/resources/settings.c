#include "contiki.h"
#include "coap-engine.h"
#include "sys/log.h"
#include <stdio.h>
//Utility functions
#include "../Utility/Timestamp/Timestamp.h"
#include "../Utility/RandomNumberGenerator/RandomNumberGenerator.h"
#include "../Utility/JSONSenML/JSONSenML.h"

//----------------------------------PARAMETERS----------------------------------//

//[+] LOG CONFIGURATION
#define LOG_MODULE "App"
#define LOG_LEVEL LOG_LEVEL_INFO
//[+] DEBUG
#define DEBUG true

//[+] TIME PARAMETERS
// For debugging purposes in deployment the advanced time is faked
#if DEBUG
  #define H_SAMPLING_PERIOD 0.0025 // in hours (corresponds to 9 seconds)
  int m_sampling_period = 1; // 1 minute but this values is not used in debug mode
#else
  #define H_SAMPLING_PERIOD 0.25// in hours
  int m_sampling_period = 60 * H_SAMPLING_PERIOD; //every 15 minutes in a real use case
#endif
int sampling_period = 3600 * H_SAMPLING_PERIOD; //in seconds

//[+] OBSERVING PARAMETERS
extern struct etimer sleep_timer; // Timer used to sleep the process

//----------------------------------RESOURCES----------------------------------//

static float h_sampling_period = H_SAMPLING_PERIOD; // in hours

//----------------------------FUNCTIONS DEFINITIONS----------------------------------//

static void res_get_handler(coap_message_t *request, coap_message_t *response,
                          uint8_t *buffer, uint16_t preferred_size, int32_t *offset);
static void res_put_handler(coap_message_t *request, coap_message_t *response,
                          uint8_t *buffer, uint16_t preferred_size, int32_t *offset);

//----------------------------RESOURCE DEFINITION----------------------------------//

RESOURCE(
        res_settings, 
        "title=\"res_settings\";rt=\"json\";if=\"sensor\"", // Parametrizable resource (sampling_period), not observable because it does not change, no notificable
        res_get_handler, 
        NULL,
        res_put_handler,
        NULL);

//----------------------------FUNCTIONS DECLARATIONS----------------------------------//

// This function is called when the resource is requested
static void res_get_handler(coap_message_t *request, coap_message_t *response,
                          uint8_t *buffer, uint16_t preferred_size, int32_t *offset){
    
    LOG_INFO("[Load-manager]//--------------NEW-GET-SETTING-REQUEST---------------------\n");
   
    // Prepare the json
    char json_str[MAX_PAYLOAD_LEN];
    int payload_len = -1;
    int value = (int)(h_sampling_period * DECIMAL_ACCURACY);
    // Create the JSON string
    snprintf(json_str, MAX_PAYLOAD_LEN, "{\"Load sampling period(h)\":%d}", value);
    // Set the payload length
    payload_len = strlen(json_str);

    // Check if the payload length is valid
    if (payload_len < 0)
    {
      LOG_ERR("\t Error in the json creation\n");
      coap_set_status_code(response, INTERNAL_SERVER_ERROR_5_00);
      coap_set_payload(response, buffer, 0);
      return;
    } else if (payload_len > preferred_size) // Payload lenght greater than buffer size
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
    LOG_INFO("[Load-manager] Sending settings: %s with size: %d\n", buffer, payload_len);
}

// This function is called when a PUT request is received
static void res_put_handler(coap_message_t *request, coap_message_t *response,
                          uint8_t *buffer, uint16_t preferred_size, int32_t *offset)
{
    LOG_INFO("[Load-manager]//--------------NEW-PUT-SETTING-REQUEST---------------------\n");

    // Get the new settings
    const uint8_t *payload = NULL;
    int payload_len = coap_get_payload(request, &payload);

    // Check if the payload is valid
    if (payload_len < 0)
    {
      LOG_ERR("\t Error in the payload\n");
      coap_set_status_code(response, BAD_REQUEST_4_00);
      coap_set_payload(response, buffer, 0);
      return;
    }   

    LOG_INFO("[Load-manager] Received settings: %s\n",(char*)payload);

    int value = 0;
    sscanf((char*)payload, "{\"Load sampling period(h)\":%d}", &value);

    LOG_INFO("[Load-manager] Received settings value: %d\n",value);
    // Get the value from the payload
    h_sampling_period = (float)(value)/DECIMAL_ACCURACY;
    // Update the sampling period
    sampling_period = 3600.0 * h_sampling_period;
    if(sampling_period < 1) {
       sampling_period = 1; // Minimum sampling period is 1 second
    }
    
    #if DEBUG
      m_sampling_period = 1; // 1 minute but this values is not used in debug mode (modification has no effect)
    #else
      m_sampling_period = 60 * h_sampling_period;
    #endif

    LOG_INFO("[Load-manager] New settings: Load sampling period(h): %f \n", h_sampling_period);

    // Prepare the response
    // CHANGED_2_04: The request has succeeded and the resource have been modified as a result of the PUT request.
    coap_set_status_code(response, CHANGED_2_04);
    // Empty payload
    coap_set_payload(response, buffer, 0);
    
}