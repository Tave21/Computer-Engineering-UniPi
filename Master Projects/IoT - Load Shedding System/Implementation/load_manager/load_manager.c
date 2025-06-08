#include "contiki.h"
#include "coap-engine.h"
#include "coap-blocking-api.h"
#include "sys/log.h"
#include "etimer.h"
#include <stdio.h>
#include "../Utility/Timestamp/Timestamp.h"
#include "../Utility/Leds/Leds.h"

//----------------------------------PARAMETERS----------------------------------//

//[+] LOG CONFIGURATION
#define LOG_MODULE "App"
#define LOG_LEVEL LOG_LEVEL_INFO

//[+] DEBUG
#define DEBUG true //if DEBUG is true, the node will register with a sampling period of x hours,
                   // otherwise it will register with a sampling period of x hours

//[+] REGISRATION PARAMETERS
#if DEBUG                                                                  
    #define BASE_INF "{\"node\":\"load-manager\",\"resource\":\"load\",\"settings\":\"{\\\"Load sampling period(h)\\\":25}\"}"
#else
    #define BASE_INF "{\"node\":\"load-manager\",\"resource\":\"load\",\"settings\":\"{\\\"Load sampling period(h)\\\":2500}\"}"
#endif

#define MAX_REGISTER_ATTEMPTS 3 // Number of attempts to register with the server
#define SERVER_EP "coap://[fd00::1]:5683" // Server endpoint
#define CREATED_CODE 65 // 2.01 --> Response code for a successful creation of a resource

int max_attempts = MAX_REGISTER_ATTEMPTS;
// Registration URL
static const char *service_registration_url = "/registration"; 

//[+] CLOCK PARAMETERS
#define MAX_CLOCK_REQ_ATTEMPTS 3 // Number of attempts to get the clock from the server
static const char *service_clock_url = "/clock"; // clock URL

//[+] TIME USER PARAMETERS
extern int m_sampling_period; //in minutes
extern int sampling_period; //in seconds

// It has to be requested to the cloud application
Timestamp timestamp = {
  .year = 2000,
  .month = 0,
  .day = 0,
  .hour = 0,
  .minute = 0
};

//[+] TIMERS
#define SLEEP_INTERVAL 30 // in seconds
struct etimer sleep_timer;

//----------------------------------RESOURCES----------------------------------//

extern coap_resource_t res_load;
extern coap_resource_t res_settings;

//----------------------------------FUNCTIONS----------------------------------//

// Define a handler to handle the response from the server
void registration_chunk_handler(coap_message_t *response)
{
    // Check if the response is NULL or if the code is not 65 (created)
    if(response == NULL) {
        LOG_ERR("[Load-manager] Request timed out\n");
    } else if (response->code != 65)
    {
        LOG_ERR("[Load-manager] Error in registration: %d\n", response->code);
    } else { // Successful registration
        LOG_INFO("[Load-manager] Successful node registration\n");        
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

// Define a handler to handle the clock response from the server
void clock_chunk_handler(coap_message_t *response)
{
    // Check if the response is NULL or if the code is not 69 (success)
    if(response == NULL) {
        LOG_ERR("Request timed out\n");
    }  else if (response->code != 69)
    {
        LOG_ERR("[Load-manager] Error clock request: %d\n",response->code);
    } else { // Successful clock request
        LOG_INFO("[Load-manager] Received clock from server: %s\n", response->payload);
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


//----------------------------MAIN PROCESS----------------------------------------//

// Process name
PROCESS(load_manager_process, "load_manager process"); 
// Autostart the process at node boot
AUTOSTART_PROCESSES(&load_manager_process); 

//--------------------------------------------------------------------------------//

// This process is responsible for managing the load manager node
// ev = event, data = data passed to the process
PROCESS_THREAD(load_manager_process, ev, data) // load_manager_process thread definition
{
  // Process initialization
  PROCESS_BEGIN(); 

  // Yellow LED is used to indicate the process is starting --> Configuration Phase
  leds_single_on(LEDS_YELLOW);

  // Activate the load resource to be accessed externally
  coap_activate_resource(&res_load, "load");

  // Structure to hold the server endpoint
  static coap_endpoint_t server_ep;

  // Structure to hold the request, used as a pointer (to avoid dynamic memory allocation)
  static coap_message_t request[1]; 
  
  // Populate the coap server endpoint structure
  coap_endpoint_parse(SERVER_EP, strlen(SERVER_EP), &server_ep);

  //------------------------[1]-Clock-Request----------------------------------//

  LOG_INFO("[Load-manager] Time request process started\n");

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
  
  //------------------------[3]-Load-Sensing----------------------------------//

  LOG_INFO("[Load-manager] Started\n");

  // Activate the settings resource 
  coap_activate_resource(&res_settings, "settings");
  // Turn the yellow LED off
  leds_single_off(LEDS_YELLOW);
  // Turn the red LED on --> Sleep Phase 
  ctrl_leds(LEDS_RED);
  // Sensing interval
  etimer_set(&sleep_timer, CLOCK_SECOND * sampling_period);
  do
  {
    // Wait for any event to occur
    PROCESS_YIELD();
    if(etimer_expired(&sleep_timer))
    {
      // Turn green LED on --> Operation Phase
      ctrl_leds(LEDS_GREEN);
      // Update the timestamp of some minutes to sync with the server
      advance_time(&timestamp, m_sampling_period);
      // Sense the load 
      res_load.trigger();
      // Wait for the next sensing interval --> Sleep Phase
      ctrl_leds(LEDS_RED);
      // Set the event timer to sleep for a while
      etimer_set(&sleep_timer, CLOCK_SECOND * sampling_period);
    }
    
  } while (1);

  PROCESS_END();
}