#ifndef JSONSENML_H
#define JSONSENML_H
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include "sys/log.h"
#include <locale.h>
#ifndef COOJA
#include <nrfx.h>
#endif

#define BASE_NAME_LEN 32
#define MAX_PAYLOAD_LEN 256
#define MAX_STR_LEN 50
#define V_INT 0
#define V_FLOAT 1
#define DECIMAL_ACCURACY 10000
//LOG CONFIGURATION
#define LOG_MODULE "App"
#define LOG_LEVEL LOG_LEVEL_INFO

typedef union {
    float v;    // float value
    int vd;     // int value
} value;

typedef struct{
    char* name;
    char* unit;
    char* time;
    value v;
    int type;
} MeasurementData;


typedef struct {
    char* base_name;
    char* base_unit;
    MeasurementData* measurement_data;
    int num_measurements;

} json_senml;

void get_base_name(char *mac_str);
int json_to_payload(json_senml* js, char* string);
int copy_value (char *string, char *output, char *start, char *end);
void print_json_senml(json_senml *senml);
void parse_str (char *payload, json_senml * js);
#endif // JSONSENML_H