# primitive conversion specifications
spec_atomic <- c(
  # Four-digit year
  "%Y" = "\\d{4}",
  # ISO 8601 week-based year (four digits)
  "%G" = "\\d{4}",
  # Two-digit year
  "%y" = "\\d{2}",
  # Century (two digits)
  "%C" = "\\d{2}",
  # ISO 8601 week-based year (two digits)
  "%g" = "\\d{2}",
  # Month as a two-digit number (01–12)
  "%m" = "\\d{2}",
  # Day of month as two digits (01–31)
  "%d" = "\\d{2}",
  # Hour (24-hour clock, 00–23)
  "%H" = "\\d{2}",
  # Hour (12-hour clock, 01–12)
  "%I" = "\\d{2}",
  # Minute (00–59)
  "%M" = "\\d{2}",
  # Second (00–59)
  "%S" = "\\d{2}",
  # ISO 8601 week number (01–53)
  "%V" = "\\d{2}",
  # Week number, Sunday as first day (00–53)
  "%U" = "\\d{2}",
  # Week number, Monday as first day (00–53)
  "%W" = "\\d{2}",
  # Day of year (001–366)
  "%j" = "\\d{3}",
  # Numeric time zone offset (+HHMM or -HHMM)
  "%z" = "[+-]\\d{4}",
  # Day of month, space- or zero-padded ( 1–31)
  "%e" = "[\\d\\s]\\d",
  # ISO weekday number (1–7, Monday = 1)
  "%u" = "\\d",
  # Weekday number (0–6, Sunday = 0)
  "%w" = "\\d",
  # Abbreviated weekday name
  "%a" = "[[:alpha:]]+",
  # Full weekday name
  "%A" = "[[:alpha:]]+",
  # Abbreviated month name
  "%b" = "[[:alpha:].]+",
  # Full month name
  "%B" = "[[:alpha:]]+",
  # Abbreviated month name (same as %b)
  "%h" = "[[:alpha:].]+",
  # Locale-specific AM/PM indicator
  "%p" = "[[:alpha:].]+",
  # Time zone name or abbreviation
  "%Z" = "[[:alpha:]]+",
  # Newline
  "%n" = "\\s+",
  # Tab
  "%t" = "\\t"
)


# compound conversion specifications
spec_compound <- c(
  # Date as MM/DD/YY
  "%D" = paste0(spec_atomic["%m"], "/", spec_atomic["%d"], "/", spec_atomic["%y"]),
  # ISO date as YYYY-MM-DD
  "%F" = paste0(spec_atomic["%Y"], "-", spec_atomic["%m"], "-", spec_atomic["%d"]),
  # Time as HH:MM (24-hour)
  "%R" = paste0(spec_atomic["%H"], ":", spec_atomic["%M"]),
  # Time as HH:MM:SS (24-hour)
  "%T" = paste0(spec_atomic["%H"], ":", spec_atomic["%M"], ":", spec_atomic["%S"]),
  # Locale date as YY/MM/DD
  "%x" = paste0(spec_atomic["%y"], "/", spec_atomic["%m"], "/", spec_atomic["%d"]),
  # Time as HH:MM:SS AM/PM (12-hour)
  "%r" = paste0(
    spec_atomic["%I"], ":", spec_atomic["%M"], ":", spec_atomic["%S"],
    "\\s+", spec_atomic["%p"]
  )
)


# Locale time representation (same as %T)
spec_compound["%X"] <- spec_compound["%T"]

# Locale date and time representation
# Example: Wed Jun  7 14:03:59 2023
spec_compound["%c"] <- paste0(
  spec_atomic["%a"], "\\s+",
  spec_atomic["%b"], "\\s+",
  spec_atomic["%e"], "\\s+",
  spec_compound["%T"], "\\s+",
  spec_atomic["%Y"]
)


# Combine all conversion specifications
conversion_specs <- c(spec_atomic, spec_compound)
