#pragma once
#include "dr_mp3.h"
#include <stdalign.h>
#include <stddef.h>
#include <stdint.h>

extern const size_t sizeof_drmp3;
static const uint32_t MAX_ALIGNMENT = alignof(max_align_t);

drmp3_uint32 drmp3_channels(drmp3 *mp3);
drmp3_uint32 drmp3_sample_rate(drmp3 *mp3);
