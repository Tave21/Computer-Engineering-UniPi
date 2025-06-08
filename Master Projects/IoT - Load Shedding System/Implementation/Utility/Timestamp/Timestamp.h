#ifndef TIMESTAMP_H
#define TIMESTAMP_H
#include <stdio.h>
#include <string.h>
#include "sys/log.h"
#define TIMESTAMP_STRING_LEN 18
typedef struct {
    int year;
    int month;
    int day;
    int hour;
    int minute;
} Timestamp;
//[+] LOG CONFIGURATION
#define LOG_MODULE "App"
#define LOG_LEVEL LOG_LEVEL_INFO
// Advance the time by the given hours
void advance_time(Timestamp* ts, int minutes);
void convert_to_feature(Timestamp* ts, float* float_ts);
void copy_timestamp(Timestamp* dest_ts, Timestamp* src_ts);
int timestamp_to_string(Timestamp* ts, char* string);
void string_to_timestamp(char* string, Timestamp* ts);

// Compare two timestamps: 0 if equal, 1 if ts1 > ts2, -1 if ts1 < ts2
int cmp_timestamp(Timestamp* ts1, Timestamp* ts2);
#endif // TIMESTAMP_H