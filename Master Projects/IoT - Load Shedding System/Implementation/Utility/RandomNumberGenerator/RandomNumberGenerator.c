#include "RandomNumberGenerator.h"

// Generate a random float between min_value and max_value
float generate_random_float(float old_value, int max_value, int min_value, float step_size, int increases)
{
    float step = step_size * (max_value - min_value);
    float random_value = (float)random_rand()/ RANDOM_RAND_MAX;
    float delta = random_value * step;
    float new_value = 0;
    if (increases)
        new_value = old_value + delta;
    else
        new_value = old_value - delta;
    // Ensures continous variation
    if (new_value > (float)max_value)
        new_value = 5.0f;
    if (new_value < (float)min_value)
        new_value = 5.0f;
    if(new_value <= 1.0f)
        new_value = 7.1f; 
    return new_value;
}
