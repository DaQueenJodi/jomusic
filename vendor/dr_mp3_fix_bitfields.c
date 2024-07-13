#include "dr_mp3_fix_bitfields.h"
#include <stdalign.h>

const size_t sizeof_drmp3 = sizeof(drmp3);
const size_t alignof_drmp3 = alignof(drmp3);

drmp3_uint32 drmp3_channels(drmp3 *mp3) {
	return mp3->channels;
}
drmp3_uint32 drmp3_sample_rate(drmp3 *mp3) {
	return mp3->sampleRate;
}
