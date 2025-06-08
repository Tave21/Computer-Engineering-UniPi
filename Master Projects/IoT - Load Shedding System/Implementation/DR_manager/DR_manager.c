#include <stdio.h>

#include "contiki.h"
#include "sys/log.h"
#include "etimer.h"
#include "os/dev/leds.h"
#include "os/dev/button-hal.h"

#include "coap-engine.h"
#include "coap-blocking-api.h"
#include "coap-observe-client.h"

#include "../Utility/RandomNumberGenerator/RandomNumberGenerator.h"
#include "../Utility/JSONSenML/JSONSenML.h"
#include "../Utility/Timestamp/Timestamp.h"
#include "../Utility/Leds/Leds.h"

//----------------------------------PARAMETERS----------------------------------//

//[+] LOG CONFIGURATION
#define LOG_MODULE "App"
#define LOG_LEVEL LOG_LEVEL_INFO

//[+] DEBUG
#define DEBUG true

//[+] REGISTRATION PARAMETERS
#if DEBUG
    #define BASE_INF "{\"node\":\"DR-manager\",\"resource\":\"status\",\"settings\":\"{\\\"Load Threshold(W)\\\":40000}\"}"
#else
    #define BASE_INF "{\"node\":\"DR-manager\",\"resource\":\"status\",\"settings\":\"{\\\"Load Threshold(W)\\\":130000}\"}"
#endif
#define MAX_REGISTER_ATTEMPTS 3
#define SERVER_EP "coap://[fd00::1]:5683"
#define CREATED_CODE 65

// The maximum number of attempts to register the node
static int max_attempts = MAX_REGISTER_ATTEMPTS;

// Registration URL
static const char *service_registration_url = "/registration"; 

//[+] DISCOVERY PARAMETERS
#define RES_LOAD "load"
#define MAX_DISCOVERY_ATTEMPTS 3
static char ip_load_manager[40];
static const char *service_discovery_url = "/discovery";

//[+] CLOCK PARAMETERS
#define MAX_CLOCK_REQ_ATTEMPTS 3
static const char *service_clock_url = "/clock";

//[+] OBSERVING PARAMETERS
static coap_endpoint_t load_manager_ep;
static coap_observee_t* load_res;

Timestamp timestamp = {
  .year = 2000,
  .month = 0,
  .day = 0,
  .hour = 0,
  .minute = 0
};

//[+] OBSERVED RESOURCE
float predicted_load = -1;

//[+] TIMERS
#define SLEEP_INTERVAL 30 // in seconds 
static struct etimer sleep_timer;

//[+] BUTTON PARAMETERS
bool button_pressed = false;

//----------------------------------RESOURCES----------------------------------//

extern coap_resource_t res_status;
extern coap_resource_t res_settings;

//----------------------------------FUNCTIONS----------------------------------//

// Define a handler to handle the response from the server
void registration_chunk_handler(coap_message_t *response)
{
    // Check if the response is NULL or if the code is not 65 (created)
    if(response == NULL) {
        LOG_ERR("[DR-manager] Request timed out\n");
    } else if (response->code != 65)
    {
        LOG_ERR("[DR-manager] Error in registration: %d\n", response->code);
    } else { // Successful registration
        LOG_INFO("[DR-manager] Successful node registration\n");
        // Stop the registration attempts
        max_attempts = 0; 
        return;
    }

    // Retry registration
    max_attempts--;

    // If max attempts are reached, signal to wait
    if(max_attempts == 0) {
        max_attempts = -1;
    }
}

// Define a handler to handle the discovery response from the server
void discovery_chunk_handler(coap_message_t *response)
{
    const uint8_t *buffer = NULL;
    // Check if the response is NULL or if the code is not 69 (success)
    if(response == NULL) {
        LOG_ERR("[DR-manager] Request timed out\n");
    } else if (response->code != 69)
    {
        LOG_ERR("[DR-manager] Error in discovery: %d\n", response->code);
    } else { // Successful discovery
        // Extract the IP address from the response payload
        coap_get_payload(response, &buffer);
        strncpy(ip_load_manager, (char *)buffer, response->payload_len);
        LOG_INFO("[DR-manager] Successful node discovery: %s\n", ip_load_manager);
        // Stop the discovery attempts
        max_attempts = 0; 
        return;
    }

    // Retry discovery
    max_attempts--;

    // If max attempts are reached, signal to wait
    if(max_attempts == 0) {
        max_attempts = -1;
    }
}

// Define a handler to handle the clock response from the server
void clock_chunk_handler(coap_message_t *response)
{
    // Check if the response is NULL or if the code is not 69 (success)
    if(response == NULL) {
        LOG_ERR("Request timed out\n");
    }  else if (response->code != 69)
    {
        LOG_ERR("[DR-manager] Error in clock request: %d\n", response->code);
    } else { // Successful clock request
        LOG_INFO("[DR-manager] Received clock from server: %s\n", response->payload);
        // Extract the timestamp from the response
        char str_timestamp[TIMESTAMP_STRING_LEN];
        strncpy(str_timestamp, (char*)response->payload, response->payload_len);
        string_to_timestamp(str_timestamp, &timestamp);
        
        // Stop the clock request attempts
        max_attempts = 0; 
        return;
    }

    // Retry clock request 
    max_attempts--;

    // If max attempts are reached, signal to wait
    if(max_attempts == 0) {
        max_attempts = -1;
    }
}

// Define a callback function to handle the load resource
static void load_callback(coap_observee_t *obs, void *notification, coap_notification_flag_t flag)
{
    LOG_INFO("[DR-manager] Notification received:\n");
    
    json_senml payload;

    MeasurementData data[2];
    payload.measurement_data = data;
    payload.num_measurements = 2;

    // Allocating memory for the name and unit
    char base_name[MAX_STR_LEN];
    char base_unit[] = "W";
    char name[2][MAX_STR_LEN];
    char unit[2][MAX_STR_LEN];
    char time[2][TIMESTAMP_STRING_LEN];

    // Set the base name and unit
    payload.base_name = base_name;
    payload.base_unit = base_unit;
    
    payload.measurement_data[0].name = name[0];
    payload.measurement_data[0].unit = unit[0];
    payload.measurement_data[0].time = time[0];

    payload.measurement_data[1].name = name[1];
    payload.measurement_data[1].unit = unit[1];
    payload.measurement_data[1].time = time[1];

    const uint8_t *buffer = NULL;

    // Check if the notification is NULL
    if(notification){
        // Extract the payload from the notification
        coap_get_payload(notification, &buffer);
    }

    // Check the notification flag
    switch (flag) {
        case NOTIFICATION_OK:
            // Parse the payload
            parse_str((char*)buffer, &payload);
            // Update the predicted load 
            predicted_load = payload.measurement_data[0].v.v;
            // Update the timestamp    
            char * timestamp_str = payload.measurement_data[0].time;
            // Remove the quotes "" in the string
            timestamp_str += 1;
            timestamp_str[strlen(timestamp_str) - 1] = '\0'; 
            string_to_timestamp(timestamp_str, &timestamp);
            // Trigger res_status event 
            res_status.trigger();
            break;
        case OBSERVE_OK:
            LOG_INFO("[DR-manager] Observe OK\n");
            break;
        case OBSERVE_NOT_SUPPORTED:
            LOG_ERR("[DR-manager] Observe not supported\n");
            break;
        case ERROR_RESPONSE_CODE:
            LOG_ERR("[DR-manager] Error response code\n");
            break;
        case NO_REPLY_FROM_SERVER:
            LOG_ERR("[DR-manager] No reply from server\n");
            break;
    }
}

//----------------------------MAIN PROCESS----------------------------------------//

// Process name
PROCESS(DR_manager_process, "DR-manager process");
// Autostart the process at node boot
AUTOSTART_PROCESSES(&DR_manager_process);

//--------------------------------------------------------------------------------//

// This process is responsible for managing the DR manager node
// ev = event, data = data passed to the process
PROCESS_THREAD(DR_manager_process, ev, data) // DR_manager_process thread definition
{
  // Process initialization
  PROCESS_BEGIN();

  // Yellow LED is used to indicate the process is starting --> Configuration Phase
  leds_single_on(LEDS_YELLOW);

  // Activate the actuator status resource
  coap_activate_resource(&res_status ,"status");
  
  // Structure to hold the server endpoint
  static coap_endpoint_t server_ep;

  // Structure to hold the request, used as a pointer (to avoid dynamic memory allocation)
  static coap_message_t request[1]; 

  // Populate the coap server endpoint structure
  coap_endpoint_parse(SERVER_EP, strlen(SERVER_EP), &server_ep);

  //------------------------[1]-Clock-Request----------------------------------//

  LOG_INFO("[DR-manager] Time request process started\n");

  // Set max attempts to the maximum number of attempts
  max_attempts = MAX_CLOCK_REQ_ATTEMPTS;

  // Try to get the clock from the server
  while (max_attempts != 0) {
      // Prepare the GET request: CON --> confirmable (ACK needed), COAP_GET --> GET method, 0 --> initial message ID (will be modified by Contiki)
      coap_init_message(request, COAP_TYPE_CON, COAP_GET, 0);
      // Set the URI path to the clock resource for accessing the server resource
      coap_set_header_uri_path(request, service_clock_url);
      // Set empty payload
      coap_set_payload(request, (uint8_t *)"", strlen(""));
      // Coap macro to send coap request and block until response is received or timeout occurs
      // clock_chunk_handler is the function that will be called when the server responds to the request
      COAP_BLOCKING_REQUEST(&server_ep, request, clock_chunk_handler);

      // If the server does not respond, the node sleeps and tries again
      if (max_attempts == -1) {
          // CLOCK_SECOND is the time unit used by Contiki OS, 1 second in clock ticks
          // Set the event timer to sleep for a while
          etimer_set(&sleep_timer, CLOCK_SECOND * SLEEP_INTERVAL);
          // Wait for the timer to expire and wake up the process
          PROCESS_WAIT_EVENT_UNTIL(etimer_expired(&sleep_timer));
          // Restart the clock attempts
          max_attempts = MAX_CLOCK_REQ_ATTEMPTS;
      }
  }

  //------------------[2]-CoAP-Server-Registration-------------------------------//

  max_attempts = MAX_REGISTER_ATTEMPTS;
  // Try to register with the server
  while (max_attempts != 0) {
      // Prepare the POST request: CON --> confirmable (ACK needed), COAP_POST --> POST method, 0 --> initial message ID (will be modified by Contiki)
      coap_init_message(request, COAP_TYPE_CON, COAP_POST, 0);
      // Set the URI path to the registration resource for accessing the server resource
      coap_set_header_uri_path(request, service_registration_url);
      // Set the payload with the node information
      coap_set_payload(request, (uint8_t *)BASE_INF, strlen(BASE_INF));
      // Coap macro to send coap request and block until response is received or timeout occurs
      // registration_chunk_handler is the function that will be called when the server responds to the request
      COAP_BLOCKING_REQUEST(&server_ep, request, registration_chunk_handler);

      // If the server does not respond, the node sleeps and tries again
      if (max_attempts == -1) {
          // CLOCK_SECOND is the time unit used by Contiki OS, 1 second in clock ticks
          // Set the event timer to sleep for a while
          etimer_set(&sleep_timer, CLOCK_SECOND * SLEEP_INTERVAL);
          // Wait for the timer to expire and wake up the process
          PROCESS_WAIT_EVENT_UNTIL(etimer_expired(&sleep_timer));
          // Restart the registration attempts
          max_attempts = MAX_REGISTER_ATTEMPTS; 
      }
  }

  //------------------------[3]-Discovery-Load-Node---------------------------------//

  LOG_INFO("[DR-manager] Discovery process started\n");
  max_attempts = MAX_DISCOVERY_ATTEMPTS;
  // Try to discover the load node
  while (max_attempts != 0) {
      // Prepare the GET request: CON --> confirmable (ACK needed), COAP_GET --> GET method, 0 --> initial message ID (will be modified by Contiki)
      coap_init_message(request, COAP_TYPE_CON, COAP_GET, 0);
      // Set the URI path to the clock resource for accessing the server resource
      coap_set_header_uri_path(request, service_discovery_url);
      // Set the payload
      coap_set_payload(request, (uint8_t *)RES_LOAD, strlen(RES_LOAD));
      // Coap macro to send coap request and block until response is received or timeout occurs
      // discovery_chunk_handler is the function that will be called when the server responds to the request
      COAP_BLOCKING_REQUEST(&server_ep, request, discovery_chunk_handler);
      // Something goes wrong with the registration, the node sleeps and tries again
      if (max_attempts == -1) {
          // CLOCK_SECOND is the time unit used by Contiki OS, 1 second in clock ticks
          // Set the event timer to sleep for a while
          etimer_set(&sleep_timer, CLOCK_SECOND * SLEEP_INTERVAL);
          // Wait for the timer to expire and wake up the process
          PROCESS_WAIT_EVENT_UNTIL(etimer_expired(&sleep_timer));
          // Restart the discovery attempts
          max_attempts = MAX_DISCOVERY_ATTEMPTS;
      }
  }

  //Register to the load resource
  char load_ep[100];
  // Create the load endpoint string
  snprintf(load_ep, 100, "coap://[%s]:5683", ip_load_manager);
  // Parse the load endpoint string
  coap_endpoint_parse(load_ep, strlen(load_ep), &load_manager_ep);

  load_res = coap_obs_request_registration(&load_manager_ep, RES_LOAD, load_callback, NULL);

  //------------------------[3]-DR-Management----------------------------------//

  LOG_INFO("[DR-manager] DR manager started\n");
  // Activate the settings resource
  coap_activate_resource(&res_settings, "settings");
  // Turn off the yellow LED
  leds_single_off(LEDS_YELLOW);
  // Turn the red LED on --> Actuator off
  ctrl_leds(LEDS_RED);
  do
  {
    // Wait for any event to occur
    PROCESS_YIELD();
    // Check if the event is a button press event
    if(ev == button_hal_press_event)
    {
        button_pressed = true;
        // Update the timestamp
        #if DEBUG
            int minutes_passed =  1;      
        #else
            // Calculate the number of minutes passed since the last sensing interval
            clock_time_t remaining_time = etimer_expiration_time(&sleep_timer) - clock_time();
            int minutes_passed =  ( sampling_period - (remaining_time / CLOCK_SECOND)) / 60; 
        #endif
        
        Timestamp old_timestamp={
            .year = timestamp.year,
            .month = timestamp.month,
            .day = timestamp.day,
            .hour = timestamp.hour,
            .minute = timestamp.minute
        };
        advance_time(&timestamp, minutes_passed);
        // Trigger res_status event 
        res_status.trigger();
        LOG_INFO("[DR-manager] Minutes passed: %d minutes\n", minutes_passed);
        // Restore the timestamp for the correct advance at the next sensing interval
        copy_timestamp(&timestamp, &old_timestamp);
        button_pressed = false;
    } 
  } while (1);

  PROCESS_END();
}
/*---------------------------------------------------------------------------*/
