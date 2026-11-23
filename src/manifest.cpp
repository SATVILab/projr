#include <cpp11.hpp>

#include <algorithm>
#include <cmath>
#include <cstring>
#include <numeric>
#include <string>
#include <vector>

// R supplies UTF-8 columns. strcmp compares their bytes independently of locale.
[[cpp11::register]]
cpp11::doubles manifest_file_order(cpp11::strings files, cpp11::strings hashes) {
  if (files.size() != hashes.size()) cpp11::stop("Filename and hash counts differ.");
  std::vector<R_xlen_t> order(files.size());
  std::iota(order.begin(), order.end(), R_xlen_t{0});
  for (auto i : order) {
    if (files[i] == NA_STRING || hashes[i] == NA_STRING)
      cpp11::stop("Missing filename or hash in manifest.");
  }
  auto compare = [&](R_xlen_t left, R_xlen_t right) {
    int diff = std::strcmp(CHAR(files[left]), CHAR(files[right]));
    return diff ? diff : std::strcmp(CHAR(hashes[left]), CHAR(hashes[right]));
  };
  std::sort(order.begin(), order.end(), [&](R_xlen_t a, R_xlen_t b) { return compare(a, b) < 0; });
  auto end = std::unique(order.begin(), order.end(), [&](R_xlen_t a, R_xlen_t b) { return compare(a, b) == 0; });

  // Double indices support long R vectors.
  cpp11::writable::doubles result(end - order.begin());
  std::transform(order.begin(), end, result.begin(), [](R_xlen_t i) { return static_cast<double>(i + 1); });
  return result;
}

static void append_quoted(std::string& output, const char* field) {
  output += '"';
  for (; *field; ++field) {
    if (*field == '"') output += '"';
    output += *field;
  }
  output += '"';
}

// Serialize one batch. R owns the binary connection, including Unicode paths on Windows.
[[cpp11::register]]
cpp11::raws manifest_file_bytes(cpp11::strings files, cpp11::strings hashes, cpp11::doubles indices) {
  if (files.size() != hashes.size()) cpp11::stop("Filename and hash counts differ.");
  std::string output;
  for (double index : indices) {
    if (!std::isfinite(index) || index < 1 || index > files.size() || index != std::floor(index))
      cpp11::stop("Invalid manifest row index.");
    R_xlen_t i = static_cast<R_xlen_t>(index) - 1;
    append_quoted(output, CHAR(files[i]));
    output += ',';
    append_quoted(output, CHAR(hashes[i]));
    output += '\n';
  }
  cpp11::writable::raws result(output.size());
  if (!output.empty()) std::memcpy(RAW(result), output.data(), output.size());
  return result;
}
