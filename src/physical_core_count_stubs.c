#define _GNU_SOURCE

#include <errno.h>
#include <sched.h>
#include <stdio.h>
#include <stdlib.h>

#include <caml/memory.h>
#include <caml/mlvalues.h>

struct verocaml_core_pair {
  long package;
  long core;
};

static int verocaml_read_nonnegative(const char *path, long *result)
{
  FILE *file = fopen(path, "r");
  if (file == NULL) return 0;

  long value = -1;
  char extra = '\0';
  int fields = fscanf(file, " %ld %c", &value, &extra);
  int close_result = fclose(file);
  if (fields != 1 || close_result != 0 || value < 0) return 0;
  *result = value;
  return 1;
}

static int verocaml_topology_pair(size_t cpu, struct verocaml_core_pair *pair)
{
  char path[160];
  int length = snprintf(path, sizeof(path),
      "/sys/devices/system/cpu/cpu%zu/topology/physical_package_id", cpu);
  if (length < 0 || (size_t)length >= sizeof(path)
      || !verocaml_read_nonnegative(path, &pair->package))
    return 0;

  length = snprintf(path, sizeof(path),
      "/sys/devices/system/cpu/cpu%zu/topology/core_id", cpu);
  if (length < 0 || (size_t)length >= sizeof(path)
      || !verocaml_read_nonnegative(path, &pair->core))
    return 0;
  return 1;
}

CAMLprim value verocaml_default_threads(value max_domains_value)
{
  CAMLparam1(max_domains_value);
  long max_domains = Long_val(max_domains_value);
  if (max_domains <= 0) CAMLreturn(Val_int(1));

  size_t cpu_count = 128;
  cpu_set_t *set = NULL;
  size_t set_size = 0;
  int affinity_ok = 0;

  while (cpu_count <= (1u << 20)) {
    set = CPU_ALLOC(cpu_count);
    if (set == NULL) break;
    set_size = CPU_ALLOC_SIZE(cpu_count);
    CPU_ZERO_S(set_size, set);
    if (sched_getaffinity(0, set_size, set) == 0) {
      affinity_ok = 1;
      break;
    }
    int error = errno;
    CPU_FREE(set);
    set = NULL;
    if (error != EINVAL) break;
    cpu_count *= 2;
  }

  if (!affinity_ok || set == NULL) CAMLreturn(Val_int(max_domains));

  int allowed = CPU_COUNT_S(set_size, set);
  if (allowed <= 0) {
    CPU_FREE(set);
    CAMLreturn(Val_int(max_domains));
  }

  struct verocaml_core_pair *pairs =
      calloc((size_t)allowed, sizeof(struct verocaml_core_pair));
  if (pairs == NULL) {
    CPU_FREE(set);
    CAMLreturn(Val_int(max_domains));
  }

  int distinct = 0;
  int topology_ok = 1;
  for (size_t cpu = 0; cpu < cpu_count && topology_ok; ++cpu) {
    if (!CPU_ISSET_S(cpu, set_size, set)) continue;
    struct verocaml_core_pair pair;
    if (!verocaml_topology_pair(cpu, &pair)) {
      topology_ok = 0;
      break;
    }
    int seen = 0;
    for (int index = 0; index < distinct; ++index) {
      if (pairs[index].package == pair.package
          && pairs[index].core == pair.core) {
        seen = 1;
        break;
      }
    }
    if (!seen) pairs[distinct++] = pair;
  }

  free(pairs);
  CPU_FREE(set);
  if (!topology_ok || distinct <= 0) CAMLreturn(Val_int(max_domains));
  CAMLreturn(Val_int(distinct < max_domains ? distinct : max_domains));
}
