// OCaml includes
extern "C" {
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/custom.h>
// #include <caml/callback.h>
#include <caml/fail.h>

#include <sys/errno.h>
} //extern C

// C++ includes
#include <cstdio>
#include <cstring>
#include <ctime>
// #include <omp.h>


constexpr long cwidth = 256L;
constexpr long cheight = 1024L;
constexpr long clength = cwidth * cheight;

inline long idx2apos (long idx, long nchunks) {
    long cnum = idx % nchunks;
    long cidx = idx / nchunks;
    return cnum * clength + cidx;
}

struct _cpp_cstdio_buffer {
    char *_buf {nullptr};
    long _len {0};
};

#define CPP_CSTDIO_BUFFER(v) (*((_cpp_cstdio_buffer**) Data_custom_val(v)))

/*
 *  cpp_add_content: add data to assembly buffer
 */
extern "C" {
value cpp_add_content(value vsrc, value vsz, value vpos, value vtgt)
{
    CAMLparam4(vsrc, vsz, vpos, vtgt);
    const long sz = Long_val(vsz);
    if (sz < 1) { return Val_long(-3); }
    const struct _cpp_cstdio_buffer *src = CPP_CSTDIO_BUFFER(vsrc);
    const long l1 = src->_len;
    if (l1 < sz) { return Val_long(-1); }   // test if enough bytes can be copied from source
    struct _cpp_cstdio_buffer *tgt = CPP_CSTDIO_BUFFER(vtgt);
    const long l2 = tgt->_len;
    const long nchunks = l2 / cheight / cwidth;
    if (nchunks * cheight * cwidth != l2) { return Val_long(-5); }
    const long pos = Long_val(vpos);
    if (pos < 0) { return Val_long(-4); }
    if (l2 < pos + sz) { return Val_long(-2); }  // test if the target can accept enough bytes
    // parallel loop
    long apos = idx2apos(pos, nchunks);
    // #pragma omp parallel for private(apos,idx) schedule(dynamic, cheight)
    for (long idx = 0; idx < sz; idx++) {
        apos = idx2apos(idx+pos, nchunks);
        tgt->_buf[apos] = src->_buf[idx];
    }
    CAMLreturn(Val_long(sz));
}

} // extern C

/*
 *  cpp_add_content: add data to assembly buffer
 */
extern "C" {
value cpp_get_content(value vsrc, value vsz, value vpos, value vtgt)
{
    CAMLparam4(vsrc, vsz, vpos, vtgt);
    const long sz = Long_val(vsz);
    if (sz < 1) { return Val_long(-3); }
    const struct _cpp_cstdio_buffer *src = CPP_CSTDIO_BUFFER(vsrc);
    const long l1 = src->_len;
    const long nchunks = l1 / cheight / cwidth;
    if (nchunks * cheight * cwidth != l1) { return Val_long(-5); }
    const long pos = Long_val(vpos);
    if (pos < 0) { return Val_long(-4); }
    if (l1 < sz + pos) { return Val_long(-1); }   // test if enough bytes can be copied from source
    struct _cpp_cstdio_buffer *tgt = CPP_CSTDIO_BUFFER(vtgt);
    const long l2 = tgt->_len;
    if (l2 < sz) { return Val_long(-2); }  // test if the target can accept enough bytes
    long apos = idx2apos(pos, nchunks);
    // #pragma omp parallel for private(apos,idx) schedule(dynamic, cheight)
    for (long idx = 0; idx < sz; idx++) {
        apos = idx2apos(idx+pos, nchunks);
        tgt->_buf[idx] = src->_buf[apos];
    }
    CAMLreturn(Val_long(sz));
}
} // extern C

/*
 *  cpp_b2s: return string of buffer
 */
extern "C" {
value cpp_b2s(value vbuf, value vsz, value vpos)
{
    CAMLparam3(vbuf, vsz, vpos);
    long sz = Long_val(vsz);
    if (sz < 1) { return caml_copy_string("E:wrong sz"); }
    long pos = Long_val(vpos);
    if (pos < 0) { return caml_copy_string("E:wrong pos"); }
    unsigned long len = caml_ba_byte_size(Caml_ba_array_val(vbuf));
    if (len < pos + sz) { return caml_copy_string("E:buf too short"); }
    const char *src = (const char *)Caml_ba_data_val(vbuf);
    CAMLreturn(caml_alloc_initialized_string(sz, src + pos));
}
} // extern C

extern "C" {
value cpp_date_ident(value unit)
{
    CAMLparam1(unit);
    std::time_t now = std::time(nullptr);
    char buf[64];
    std::strftime(buf, sizeof(buf), "%F", std::gmtime(&now));
    std::snprintf(buf+10, sizeof(buf)-10, "_%ld", now);
    return caml_copy_string(buf);
}
} // extern C
