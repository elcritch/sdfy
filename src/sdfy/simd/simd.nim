
when not defined(sdfyNoSimd):
  when defined(arm) or defined(arm64) or defined(aarch64):
    import ./roundedbox_neon
  else:
    import ./roundedbox_sse # I don't exist yet!
