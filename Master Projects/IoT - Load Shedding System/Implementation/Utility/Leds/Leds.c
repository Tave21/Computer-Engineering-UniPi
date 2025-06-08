#include "Leds.h"

void ctrl_leds(int color)
{       
    leds_off(LEDS_ALL);
    #ifdef COOJA
        leds_single_on(color);
    #else
        leds_on(color);
    #endif
}