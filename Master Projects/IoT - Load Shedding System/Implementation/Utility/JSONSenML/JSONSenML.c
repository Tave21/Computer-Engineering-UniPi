#include "JSONSenML.h"

// This function sets a symbolic base name for the JSON SenML payload
void get_base_name(char *str) {
    snprintf(str, BASE_NAME_LEN, "node:");
}

// This function converts the JSON SenML structure to a JSON payload string
int json_to_payload(json_senml* js, char* payload)
{
    char com[MAX_PAYLOAD_LEN] = "";
    // Save the current locale (checks the number format)
    char *current_locale = setlocale(LC_NUMERIC, NULL);
    // Set the locale to "C" to avoid problems with the decimal separator
    setlocale(LC_NUMERIC, "C");

    // Create the base name with the MAC address
    sprintf(payload, "[{\"bn\":\"%s\",\"bu\":\"%s\"},", js->base_name, js->base_unit);
    for (int i = 0; i < js->num_measurements; i++)
    {
        sprintf(com, "{\"n\":\"%s\",\"u\":\"%s\",\"t\":\"%s\",\"v\":", 
         js->measurement_data[i].name, 
         js->measurement_data[i].unit, 
         js->measurement_data[i].time);

        strcat(payload, com);

        if (js->measurement_data[i].type == V_FLOAT)
        {
            int value = (int)(js->measurement_data[i].v.v * DECIMAL_ACCURACY);
            sprintf(com, "%d}", value);
        } 
        else if (js->measurement_data[i].type == V_INT)
        {
            sprintf(com, "%d}", (int)js->measurement_data[i].v.v);
        } 
        else {
            return -1;
        }
        if (i < js->num_measurements - 1)
        {
            strcat(com, ",");
        }
        strcat(payload, com);
    }
    strcat(payload, "]");
    // Restore the original locale
    setlocale(LC_NUMERIC, current_locale);
    return strlen(payload);
}

// This function copies a value from the JSON SenML payload to the output string
int copy_value (char *string, char *output, char *start, char *end)
{

  char *start_ptr = strstr(string, start);
  char *end_ptr = strstr(string, end);

  // Check if the start and end pointers are valid
  if (start_ptr == NULL || end_ptr == NULL)
  {
	  return -1;
  }

  // Discard the start part
  start_ptr += strlen(start);
  
  // Copy the value between start and end
  strncpy(output, start_ptr, end_ptr - start_ptr);
  // Add a null terminator to the output string
  output[end_ptr - start_ptr] = '\0';
  // Return the length of the copied value
  return (end_ptr - string) + strlen(end);
}

// This function parses the JSON SenML payload and fills the json_senml structure
void parse_str (char *payload, json_senml* js)
{
  char* pos = payload;
  int step = 0;

  // Copy the base name
  step = copy_value(pos, js->base_name, "\"bn\":", ",");
  // Check if the base name was parsed correctly
  if(step == -1)
  {
      LOG_ERR("ERROR: Parsing base name\n");
      LOG_ERR("\t %s\n", pos);
      return;
  }
  // Move the position to the end of the base name
  pos += step;
  
  // Copy the base unit
  step = copy_value(pos, js->base_unit, "\"bu\":", "}");
  // Check if the base unit was parsed correctly
  if(step == -1)
  {
      LOG_ERR("ERROR: Parsing base unit\n");
      return;
  }
  // Move the position to the end of the base unit
  pos += step;

  // Skip the ",{"
  pos+=2;

  // Copy the number of measurements
  for(int i=0; i<js->num_measurements;i++)
  {
      // Copy the measurement name
      step = copy_value(pos, js->measurement_data[i].name, "\"n\":", ",");
      // Check if the measurement name was parsed correctly
      if(step == -1)
      {
        LOG_ERR("ERROR: Parsing name of measure %d\n", i);
        return;
      }
      // Move the position to the end of the measurement name
      pos += step;

      // Copy the measurement unit
      step = copy_value(pos, js->measurement_data[i].unit, "\"u\":", ",");
      // Check if the measurement unit was parsed correctly
      if(step == -1)
      {
        LOG_ERR("ERROR: Parsing unit of measure %d\n", i);
        return;
      }
      // Move the position to the end of the measurement unit
      pos += step;

      // Copy the measurement time
      step = copy_value(pos, js->measurement_data[i].time, "\"t\":", ",");
      // Check if the measurement time was parsed correctly
      if(step == -1)
      {
        LOG_ERR("ERROR: Parsing time of measure %d\n", i);
        return;
      }
      // Move the position to the end of the measurement time
      pos += step;
      
      // Copy the measurement value
      char time_value[MAX_STR_LEN];
      char *endptr;
      step = copy_value(pos, time_value, "\"v\":", "}");
      if(step == -1)
      {
        LOG_ERR("ERROR: Parsing value of measure %d\n", i);
        return;
      }
      // Move the position to the end of the measurement value
      pos += step;

      // Convert to int
      int value = strtol(time_value, &endptr, 10);

      // Check if the conversion was successful 
      if (endptr == time_value) {
         LOG_ERR("ERROR: Convert value of measure %d\n", i);
      }

      if(value >= DECIMAL_ACCURACY)
      {     // Convert to float
            js->measurement_data[i].v.v = (float)value / (float)DECIMAL_ACCURACY;
            js->measurement_data[i].type = V_FLOAT;
      } else {
            // Convert to int
            js->measurement_data[i].v.vd = value;
            js->measurement_data[i].type = V_INT;
      }

      // Skip the ",{"
      pos+=2;
  }
}

// This function prints the JSON SenML structure to the log
void print_json_senml(json_senml *senml) {
    LOG_INFO("Base Name: %s\n", senml->base_name);
    LOG_INFO("Base Unit: %s\n", senml->base_unit);
    LOG_INFO("Number of Measurements: %d\n", senml->num_measurements);

    for (int i = 0; i < senml->num_measurements; i++) {
        MeasurementData *md = &senml->measurement_data[i];
        LOG_INFO("\nMeasurement %d:\n", i + 1);
        LOG_INFO("  Name: %s\n", md->name);
        LOG_INFO("  Unit: %s\n", md->unit);
        LOG_INFO("  Time: %s\n", md->time);

       
        switch (md->type) {
                case V_FLOAT:
                    LOG_INFO("  Value: %f (float)\n", md->v.v);
                    break;
                case V_INT:
                    LOG_INFO("  Value: %d (int)\n", md->v.vd);
                    break;
                 default:
                    LOG_INFO("  Value: Unknown type\n");
                    break;
        }
    }
}

