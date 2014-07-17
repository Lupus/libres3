/* MurmurHash3 was written by Austin Appleby, and is placed in the public
 * domain. The author hereby disclaims copyright to this source code. */

#include <stdint.h>
#include <stdlib.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <assert.h>

static uint64_t MurmurHash64(const void *key, size_t len, unsigned int seed)
{
	const unsigned int m = 0x5bd1e995;
	const int r = 24;
	unsigned int h1 = seed ^ len;
	unsigned int h2 = 0;
	unsigned int k1, k2;
	uint64_t h;
	const unsigned char * data = (const unsigned char *)key;

    while(len >= 8) {
	k1  = data[0];
	k1 |= data[1] << 8;
	k1 |= data[2] << 16;
	k1 |= data[3] << 24;
	k1 *= m; k1 ^= k1 >> r; 
	k1 *= m; h1 *= m;
	h1 ^= k1;
	data += 4;
	len -= 4;

	k2  = data[0];
	k2 |= data[1] << 8;
	k2 |= data[2] << 16;
	k2 |= data[3] << 24;
	k2 *= m; k2 ^= k2 >> r; 
	k2 *= m; h2 *= m;
	h2 ^= k2;
	data += 4;
	len -= 4;
    }

    if(len >= 4) {
	k1  = data[0];
	k1 |= data[1] << 8;
	k1 |= data[2] << 16;
	k1 |= data[3] << 24;
	k1 *= m; k1 ^= k1 >> r; 
	k1 *= m; h1 *= m;
	h1 ^= k1;
	data += 4;
	len -= 4;
    }

    switch(len) {
	case 3: h2 ^= data[2] << 16;
	case 2: h2 ^= data[1] << 8;
	case 1: h2 ^= data[0];
		h2 *= m;
    };

    h1 ^= h2 >> 18; h1 *= m;
    h2 ^= h1 >> 22; h2 *= m;
    h1 ^= h2 >> 17; h1 *= m;
    h2 ^= h1 >> 19; h2 *= m;

    h = h1;
    h = (h << 32) | h2;
    return h;
}

value ml_murmurhash64(value data, value seed)
{
    CAMLparam2(data, seed);
    int64_t ret;
    assert(Is_block(data) && Tag_val(data) == String_tag);
    assert(Is_long(seed));
    ret = MurmurHash64(String_val(data), caml_string_length(data), Int_val(seed));
    CAMLreturn(caml_copy_int64(ret));
}
