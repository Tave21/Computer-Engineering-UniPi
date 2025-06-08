#include "contiki.h"
#include "coap-engine.h"
#include "sys/log.h"
#include "os/dev/leds.h"
#include <stdio.h>
#include <stdlib.h>
#include "../Utility/RandomNumberGenerator/RandomNumberGenerator.h"
#include "../Utility/JSONSenML/JSONSenML.h"
#include "../Utility/Timestamp/Timestamp.h"
#include "../Utility/Leds/Leds.h"

//----------------------------------PARAMETERS----------------------------------//

//[+] LOG CONFIGURATION
#define LOG_MODULE "App"
#define LOG_LEVEL LOG_LEVEL_INFO

#define UNIT "B" //boolean
//[+] OBSERVED PARAMETERS
extern float predicted_load;
extern Timestamp timestamp;

//[+] EXTERNAL PARAMETERS
extern bool button_pressed;
extern float threshold_user;

//[+] DECISION PARAMETERS
static int status = 0;

//----------------------------FUNCTIONS DEFINITIONS----------------------------------//
static void manage_shed_load_relay();
void res_get_handler(coap_message_t *request, coap_message_t *response,
                          uint8_t *buffer, uint16_t preferred_size, int32_t *offset);
static void res_event_handler(void);
void res_put_handler(coap_message_t *request, coap_message_t *response,
                          uint8_t *buffer, uint16_t preferred_size, int32_t *offset);

//----------------------------RESOURCE DEFINITION----------------------------------//
// The resource is observable and it is used to notify the observers when the relay is activated
EVENT_RESOURCE(
        res_status, 
        "title=\"status\";rt=\"json\";if=\"actuator\";obs", 
        res_get_handler, 
        NULL,
        res_put_handler,
        NULL,
        res_event_handler); 

//----------------------------FUNCTIONS DECLARATIONS----------------------------------//

// This function manages the shed load relay when the predicted load event happens
static void manage_shed_load_relay()
{
    if(predicted_load == -1)
    {
        LOG_ERR("[DR-manager] Load parameters not arrived\n");
        return;
    }  

    LOG_INFO("[DR-manager] Predicted load: %f, Threshold: %f\n", predicted_load, threshold_user);

    // If the predicted load is greater than the threshold_user, the relay is activated
    if( predicted_load > threshold_user && !status) {
        status = 1;
        ctrl_leds(LEDS_BLUE);
        LOG_INFO("[DR-manager] Shed load relay ON\n");
        // Notify the observers only if the relay was activated
        coap_notify_observers(&res_status);
    } else
    {
        // Check relay status
        if(status && predicted_load <= threshold_user)
        {
            // If the predicted load is less than the threshold_user, the relay is deactivated
            status = 0;
            ctrl_leds(LEDS_RED);
            LOG_INFO("[DR-manager] Shed load relay OFF\n");
            // Notify the observers only if the relay was deactivated
            coap_notify_observers(&res_status);
        }
    }
}

void res_put_handler(coap_message_t *request, coap_message_t *response,
                          uint8_t *buffer, uint16_t preferred_size, int32_t *offset) {

    
    LOG_INFO("[DR-manager]//--------------NEW-PUT-RELAY STATUS-REQUEST---------------------\n");

    // Get the new status
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

    int value = 0;
    // Get the value from the payload
    sscanf((char*)payload, "{\"status\": %d}", &value);
    
    // Check if the value is valid
    if (value != 0 && value != 1)
    {
      LOG_ERR("\t Error in the value\n");
      coap_set_status_code(response, BAD_REQUEST_4_00);
      coap_set_payload(response, buffer, 0);
      return;
    } 
    // Update the status 
    status = value;
    if (value == 1) {
        // Turn on Blue LED --> Relay ON
        ctrl_leds(LEDS_BLUE);
    } else {
        // Turn off Blue LED and turn on the Red LED --> Relay OFF
        ctrl_leds(LEDS_RED);
    }
    
    LOG_INFO("[DR-manager] New settings: Status: %d\n", status);

    // Prepare the response
    // CHANGED_2_04: The request has succeeded and the resource have been modified as a result of the PUT request.
    coap_set_status_code(response, CHANGED_2_04);
    // Empty payload
    coap_set_payload(response, buffer, 0);
    // Notify the observers
    coap_notify_observers(&res_status);
}

// This function is called when the resource is requested
void res_get_handler(coap_message_t *request, coap_message_t *response,
                          uint8_t *buffer, uint16_t preferred_size, int32_t *offset) {

    LOG_INFO("[DR-manager]//--------------NEW-GET-RELAY STATUS-REQUEST---------------------\n");

    MeasurementData data[1];
    json_senml js_senml;
    char names[1][MAX_STR_LEN] = {"status"};
    char timestamp_str[1][TIMESTAMP_STRING_LEN] = {""};
    char base_name[BASE_NAME_LEN];
    int payload_len = 0;
    
    // Create a timestamp for the current measurement
    timestamp_to_string(&timestamp, timestamp_str[0]);
    // Create the base name
    get_base_name(base_name);
     
    // Initialize the measurement data
    data[0].name = names[0];
    data[0].unit = UNIT;
    data[0].time = timestamp_str[0];
    data[0].v.v = status;
    data[0].type = V_INT;

    // Create the JSON SenML structure
    js_senml.base_name = base_name;
    js_senml.base_unit = UNIT;
    js_senml.measurement_data = data;
    js_senml.num_measurements = 1;

    // Convert the JSON SenML to a payload
    payload_len = json_to_payload(&js_senml, (char*)buffer);
   
    // Check if the payload is valid
    if (payload_len < 0)
    {
      LOG_ERR("[DR-manager] Error in the json_to_payload function\n");
      coap_set_status_code(response, INTERNAL_SERVER_ERROR_5_00);
      coap_set_payload(response, buffer, 0);
      return;
    } else if (payload_len > preferred_size) // Check if the buffer is big enough
    {
      LOG_ERR("[DR-manager] Buffer overflow\n");
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
    LOG_INFO("[DR-manager] Sending status: %s with size: %d\n", buffer, payload_len);
}

static void res_event_handler(void) {
    char timestamp_str[TIMESTAMP_STRING_LEN];
    timestamp_to_string(&timestamp, timestamp_str);

    LOG_INFO("[DR-manager]------------------------NEW-EVENT------------------ \n");

    // Button pressed event
    if(button_pressed){
        LOG_INFO("[DR-Manager] Status changed at time %s\n", timestamp_str);
        if(!status){
            status = 1;
            // Turn on Blue LED --> Relay ON
            ctrl_leds(LEDS_BLUE);
        } else {
            status = 0;
            // Turn off Blue LED and turn on the Red LED --> Relay OFF
            ctrl_leds(LEDS_RED);
        }
        
        // Notify the observers
        coap_notify_observers(&res_status);
    } else { // Load prediction event
        LOG_INFO("[DR-Manager] New load prediction received at time %s\n", timestamp_str);
        manage_shed_load_relay();
    }
    
}
