def extension_flags(lang, exts):
    return (["-X" + lang] + ["-X" + ext for ext in exts])

def flag_set(flag_config, flags):
    all_flags = []
    for f in flags:
        if flag_config.get(f):
            all_flags.extend(flag_config[f])
    return all_flags

def cpp_flags(flags):
    return ["-D" + f for f in flags]

def cpp_flags_when(is_enabled, cpp_fs):
    if is_enabled:
      return cpp_flags(cpp_fs)
    else:
      return []
