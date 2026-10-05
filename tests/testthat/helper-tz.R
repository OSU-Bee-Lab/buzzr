# Set the session time zone (via TZ) until the calling test finishes
local_tz <- function(tz, env = parent.frame()) {
  withr::local_timezone(tz, .local_envir = env)
}
