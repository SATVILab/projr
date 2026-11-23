#include <cpp11.hpp>
#include <cstdio>
#include <memory>

#ifdef _WIN32
#include <windows.h>
#include <string>
#endif

extern "C" {
#include "md5.h"
}

static std::FILE* open_binary(cpp11::r_string path) {
#ifdef _WIN32
  const std::string utf8 = path;
  int size = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, utf8.c_str(), -1, nullptr, 0);
  if (!size) return nullptr;
  std::wstring wide(size, L'\0');
  if (!MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, utf8.c_str(), -1, &wide[0], size))
    return nullptr;
  return _wfopen(wide.c_str(), L"rb");
#else
  return std::fopen(cpp11::safe[Rf_translateChar](path), "rb");
#endif
}

[[cpp11::register]]
cpp11::strings hash_files_cpp(cpp11::strings paths) {
  cpp11::writable::strings hashes(paths.size());
  unsigned char buffer[65536];
  const char hex[] = "0123456789abcdef";

  for (R_xlen_t i = 0; i < paths.size(); ++i) {
    auto path = paths[i];
    if (path == NA_STRING) cpp11::stop("Could not hash file: NA");
    std::unique_ptr<std::FILE, int (*)(std::FILE*)> file(open_binary(path), std::fclose);
    if (!file) cpp11::stop("Could not hash file: %s", CHAR(path));

    MD5_CTX context;
    projr_md5_init(&context);
    size_t count;
    do {
      cpp11::check_user_interrupt();
      count = std::fread(buffer, 1, sizeof(buffer), file.get());
      projr_md5_update(&context, buffer, count);
    } while (count == sizeof(buffer));
    if (std::ferror(file.get())) cpp11::stop("Could not hash file: %s", CHAR(path));

    unsigned char digest[16];
    char result[33] = {};
    projr_md5_final(digest, &context);
    for (size_t j = 0; j < sizeof(digest); ++j) {
      result[j * 2] = hex[digest[j] >> 4];
      result[j * 2 + 1] = hex[digest[j] & 15];
    }
    hashes[i] = result;
  }
  return hashes;
}
