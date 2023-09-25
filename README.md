About difftools

difftools aims to help with the differentiation of iPSCs into dopaminergic neurons (DaNs).
It is composed of two programs, mkmed and diffd, which compute medium composition and date / day of differentiation.
The implementation of difftools is based on:
- the differentiation protocol published by Kriks et al. (PMID: 19731555).
- the reagents and stock concentrations in use in the RWM lab.
In case you use a different protocol or different reagents / stock concentrations, difftools is not for you.

###########################

mkmed

mkmed returns the composition of your culture medium, provided a day of differentiation and a required volume.
If split and/or Anti-Anti are set to "yes", the medium composition also includes ROCKi and Anti-Anti.
If requested, mkmed also returns the recipe of the basal media required to prepare the medium.

Input:

day of differentiation day of kriks differentiation protocol (required).
Valid values include any negative or positive integer, including 0 (e.g.: -2, 60).

volume (ml) volume of culture medium in milliliters (required).
Valid values include any strictly positive floating point number (e.g.: 18.5, 230).

split cells? indicates if ROCKi should be included (default is "no").

use Anti-Anti? indicates if Anti-Anti should be included (default is "no").

display basal media recipe if ticked, returns the recipe of the required basal media (optional).

###########################

diffd

diffd returns a target differentiation day or date, provided a reference date, a reference day and a target date or day.
If requested, a differentiation calendar is generated.

Input:

reference date reference date of differentiation matching the reference day (required).
Valid values are in the form YYYY-MM-DD or YYYY.MM.DD (e.g.: 2022.10.18, 2023-05-23).
Alternatively, the string "today" can be provided and will be converted into the system's idea of the current date.

reference day reference day of differentiation matching the reference date (required).
Valid values include any negative or positive integer (e.g.: -5, 85), or days in expansion style format (e.g.: 10+1, 10+15).

target date target date for which a differentiation date is requested (mutually exclusive with target day).
Valid values are in the form YYYY-MM-DD or YYYY.MM.DD (e.g.: 2022.09.28, 2023-03-06).
Alternatively, the string "today" can be provided and will be converted into the system's idea of the current date.

target day target day for which a differentiation date is requested (mutually exclusive with target date).
Valid values include any negative or positive integer (e.g.: 3, 22), or days in expansion style format (e.g.: 10+1, 10+15).

expansion? indicates if an expansion phase should be included (default is "yes").
By default, the duration of the expansion phase is 21 days.

special expansion? if set to "yes", opens a new box to provide the duration of the expansion phase manually.

expansion duration duration of the expansion phase in days (optional).
Valid values include any strictly positive whole number (e.g.: 20, 23)

display calendar if ticked, returns a complete differentiation calendar.
Note that a differentiation calendar can be obtained by providing only a reference date and a reference day (no target).
The first day of the calendar is the lowest value between -2, reference day and target day (if provided).
The last day is the highest value between 60, reference day and target day (if provided).
