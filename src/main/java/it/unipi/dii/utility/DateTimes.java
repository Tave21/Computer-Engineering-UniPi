package it.unipi.dii.utility;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.*;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.Date;
import java.util.Objects;

public class DateTimes {

    /**
     * @param birthDate The date of birth.
     * @return True if the person have more than 18 years.
     */
    public static boolean isAdult(LocalDate birthDate) {
        return differenceDays(birthDate.toString() , getCurrentDateString()) >= 18;
    }

    /**
     * From a string formatted like date, it returns an object Date created from the date inside the input string.
     *
     * @param dateString A string formatted like "YYYY-MM-DD"
     * @return An object Date.
     */
    public static Date stringToDate(String dateString) {
        //"2024-01-25"
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
        try {
            return sdf.parse(dateString);
        } catch (ParseException e) {
            return null;
        }

    }

    /**
     * From a string formatted like timestamp, it returns an object Instant created from the timestamp inside the input string.
     *
     * @param dateString A string formatted like "YYYY-MM-DDThh:mm:ssZ"
     * @return An object Instant.
     */
    public static Instant stringToTimestamp(String dateString) {
        return Instant.parse(dateString);
    }

    /**
     * @param dateBefore Previous date.
     * @param dateAfter  After date.
     * @return The difference in days between the two dates, if the result is negative then dateAfter is previous of dateBefore.
     */
    public static long differenceDays(Date dateBefore, Date dateAfter) {
        if (dateBefore == null || dateAfter == null) {
            return 0;
        }
        long ret = (dateAfter.getTime() - dateBefore.getTime()) / 1000L;
        return ret / (3600 * 24);
    }

    /**
     * @param dateBefore String formatted like "YYYY-MM-DD"
     * @param dateAfter  String formatted like "YYYY-MM-DD"
     * @return The difference in days between the two dates, if the result is negative then dateAfter is previous of dateBefore.
     */
    public static long differenceDays(String dateBefore, String dateAfter) {
        return differenceDays(Objects.requireNonNull(stringToDate(dateBefore)), Objects.requireNonNull(stringToDate(dateAfter)));
    }

    /**
     * @param instantBefore Previous timestamp.
     * @param instantAfter  After timestamp.
     * @return The difference in days between the two timestamps, if the result is negative then dateAfter is previous of dateBefore.
     */

    public static long differenceDays(Instant instantBefore, Instant instantAfter) {
        return Math.abs(ChronoUnit.DAYS.between(instantBefore, instantAfter));
    }

    /**
     * @param instantBefore Previous timestamp.
     * @param instantAfter  After timestamp.
     * @return The difference in seconds between the two timestamps, if the result is negative then dateAfter is previous of dateBefore.
     */

    public static long differenceSeconds(Instant instantBefore, Instant instantAfter) {
        return Math.abs(ChronoUnit.SECONDS.between(instantBefore, instantAfter));
    }



    /**
     * Adds the hours, minutes and second part.
     *
     * @param inputDate String formatted like "YYYY-MM-DD"
     * @param hours     Hours to add.
     * @param minutes   Minutes to add.
     * @param seconds   Second to add.
     * @return The same date but in the timestamp format "YYYY-MM-DDThh:mm:ssZ".
     */
    public static String dateToTimestamp(String inputDate, int hours, int minutes, int seconds) {
        LocalDate formattedDate = LocalDate.parse(inputDate, DateTimeFormatter.ofPattern("yyyy-MM-dd"));
        LocalDateTime completeDate = LocalDateTime.of(formattedDate, LocalTime.of(hours, minutes, seconds));
        return completeDate.atOffset(ZoneOffset.UTC).format(DateTimeFormatter.ISO_INSTANT);
    }

    /**
     *@return An Instant object that represent the current timestamp.
     */

    public static Instant getCurrentInstant(){
        return Instant.now().plusSeconds(3600).truncatedTo(ChronoUnit.SECONDS);
    }

    /**
     *@return A String that represent the current timestamp in format.
     */

    public static String getCurrentInstantString(){
        return getCurrentInstant().toString();
    }

    /**
     *@return A LocalDate object that represent the current date.
     */

    public static LocalDate getCurrentDate(){
        return getCurrentInstant().atZone(ZoneId.systemDefault()).toLocalDate();
    }

    /**
     *@return A String that represent the current date.
     */

    public static String getCurrentDateString(){
        return getCurrentDate().toString();
    }

}

