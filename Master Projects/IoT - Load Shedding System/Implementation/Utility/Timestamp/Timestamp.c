#include "Timestamp.h"

//Advance the time by a certain number of minutes
void advance_time(Timestamp* ts, int minutes)
{
  int hours = 0;
  ts->minute += minutes;
  hours = ts->minute / 60;
  ts->minute = ts->minute % 60;

  ts->hour = ts->hour + hours;
  if (ts->hour == 24)
  {
    ts->hour = 0;
    ts->day++;
    if (ts->month == 2 && ts->day == 28) // February
    {
      ts->day = 1;
      ts->month++;
  
    }
    else if ((ts->month == 4 || ts->month == 6 || ts->month == 9 || ts->month == 11) && ts->day == 31) // April, June, September, November
    {
      ts->day = 1;
      ts->month++;
    }
    else if (ts->day == 32) // January, March, May, July, August, October, December
    {
      ts->day = 1;
      ts->month++;
      if (ts->month == 13)
      {
        ts->month = 1;
        ts->year++;
      }
    }
  }
}

void convert_to_feature(Timestamp* ts, float* float_ts)
{
  float_ts[0] = ts->month;
  float_ts[0] = ts->day;
  float_ts[1] = ts->hour;
  float_ts[2] = ts->minute;
}

void copy_timestamp(Timestamp* dest_ts, Timestamp* src_ts)
{
  dest_ts->year = src_ts->year;
  dest_ts->month = src_ts->month;
  dest_ts->day = src_ts->day;
  dest_ts->hour = src_ts->hour;
  dest_ts->minute = src_ts->minute;
}

int timestamp_to_string(Timestamp* ts, char* string)
{
  static char str_month[13];
  static char str_day[13];
  static char str_hour[13];
  static char str_minute[13];
  if (ts->month < 10)
    sprintf(str_month, "0%d", ts->month);
  else
    sprintf(str_month, "%d", ts->month);

  if (ts->day < 10)
    sprintf(str_day, "0%d", ts->day);
  else
    sprintf(str_day, "%d", ts->day);

  if (ts->hour < 10)
    sprintf(str_hour, "0%d", ts->hour);
  else
    sprintf(str_hour, "%d", ts->hour);
  
  if (ts->minute < 10)
    sprintf(str_minute, "0%d", ts->minute);
  else
    sprintf(str_minute, "%d", ts->minute);
  
  sprintf(string, "%d-%s-%sT%s:%sZ", ts->year, str_month, str_day, str_hour, str_minute);
  strcat(string, "\0");
  return strlen(string);
}

void string_to_timestamp(char* string, Timestamp* ts)
{
  int result = sscanf(string, "%d-%d-%dT%d:%dZ", &ts->year, &ts->month, &ts->day, &ts->hour, &ts->minute);
  if (result == 5) {
        LOG_DBG("Parsed successfully\n");
    } else {
        LOG_DBG("Failed to parse the string. Only %d values were read.\n", result);
    }
}

int cmp_timestamp(Timestamp* ts1, Timestamp* ts2)
{
  if (ts1->year == ts2->year && ts1->month == ts2->month && ts1->day == ts2->day && ts1->hour == ts2->hour && ts1->minute == ts2->minute)
    return 0;
  else if (ts1->year > ts2->year || (ts1->year == ts2->year && ts1->month > ts2->month) || (ts1->year == ts2->year && ts1->month == ts2->month && ts1->day > ts2->day) || (ts1->year == ts2->year && ts1->month == ts2->month && ts1->day == ts2->day && ts1->hour > ts2->hour) || (ts1->year == ts2->year && ts1->month == ts2->month && ts1->day == ts2->day && ts1->hour == ts2->hour && ts1->minute > ts2->minute))
    return 1;
  else
    return -1;
}