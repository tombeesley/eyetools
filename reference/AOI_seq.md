# Sequence analysis of area of interest entries

Analyses the sequence of entries into defined AOI regions across trials.
Can only be used with fixation data with a "fix_n" column denoting
fixation events. Assumes that AOIs are non-overlapping and hasn't been
tested with overlapping AOIs. Consecutive fixations within an AOI are
grouped together as a single entry. Non-consecutive fixations in the
same AOI (i.e., with an intervening fixation in no AOI) are treated as
two separate entries.

## Usage

``` r
AOI_seq(data, AOIs, progress = TRUE)
```

## Arguments

- data:

  A dataframe with fixation data (from fixation_dispersion). Either
  single or multi participant data

- AOIs:

  A dataframe of areas of interest (AOIs), with one row per AOI (name,
  x, y, width_radius, height).

- progress:

  Display a progress bar

## Value

a dataframe containing the sequence of entries into AOIs on each trial,
entry/exit/duration time into AOI

## Examples

``` r
# \donttest{
data <- combine_eyes(HCL)
fix_d <- fixation_dispersion(data)

AOI_seq(fix_d, AOIs = HCL_AOIs)
#>     pID trial    AOI start   end duration entry_n
#> 1   118     1 centre   437   750      313       1
#> 2   118     1   left   783  1183      400       2
#> 3   118     1 centre  1210  2190      980       3
#> 4   118     1  right  2236  2693      457       4
#> 5   118     1 centre  2743  3676      933       5
#> 6   118     1  right  3726  4223      497       6
#> 7   118     1 centre  4269  5339     1070       7
#> 8   118     1   left  5379  6792     1413       8
#> 9   118     1   left  7632  9295     1663       9
#> 10  118     1 centre  9325 10439     1114      10
#> 11  118     1   left 10495 10705      210      11
#> 12  118     1 centre 10735 12142     1407      12
#> 13  118     1  right 12185 12885      700      13
#> 14  118     1 centre 13018 13461      443      14
#> 15  118     2  right   396  1146      750       1
#> 16  118     2 centre  1199  1986      787       2
#> 17  118     2   left  2019  2629      610       3
#> 18  118     2 centre  2676  5142     2466       4
#> 19  118     2  right  5182  5342      160       5
#> 20  118     2   left  5389  5765      376       6
#> 21  118     2 centre  5912  6232      320       7
#> 22  118     2   left  6282  6752      470       8
#> 23  118     2 centre  6785  7199      414       9
#> 24  118     2  right  7249  7635      386      10
#> 25  118     3 centre   393   936      543       1
#> 26  118     3  right   976  1203      227       2
#> 27  118     3   left  1250  1830      580       3
#> 28  118     3 centre  1863  2989     1126       4
#> 29  118     3  right  3033  3549      516       5
#> 30  118     3 centre  3863  5193     1330       6
#> 31  118     4  right   186   543      357       1
#> 32  118     4 centre   576   920      344       2
#> 33  118     4   left  1110  1543      433       3
#> 34  118     4  right  1603  2389      786       4
#> 35  118     4   left  2439  3353      914       5
#> 36  118     4 centre  3393  4159      766       6
#> 37  118     4  right  4206  4593      387       7
#> 38  118     4   left  4779  5459      680       8
#> 39  118     4 centre  5506  6166      660       9
#> 40  118     4  right  6209  8009     1800      10
#> 41  118     4   left  8062  8605      543      11
#> 42  118     4  right  8662  8919      257      12
#> 43  118     4 centre  8962  9908      946      13
#> 44  118     5   left   186   439      253       1
#> 45  118     5 centre   463   769      306       2
#> 46  118     5  right  1106  1666      560       3
#> 47  118     5 centre  1706  2609      903       4
#> 48  118     5  right  2649  2999      350       5
#> 49  118     5 centre  3036  3686      650       6
#> 50  118     5  right  3729  3926      197       7
#> 51  118     5 centre  3972  4276      304       8
#> 52  118     6 centre   340   580      240       1
#> 53  118     6  right   627  1160      533       2
#> 54  118     6   left  1210  1526      316       3
#> 55  118     6 centre  1710  3330     1620       4
#> 56  118     6  right  3380  3759      379       5
#> 57  118     6   left  3813  4289      476       6
#> 58  118     6 centre  4319  5076      757       7
#> 59  119     1 centre   429   963      534       1
#> 60  119     1   left  1003  1389      386       2
#> 61  119     1 centre  1579  1862      283       3
#> 62  119     1  right  2099  3026      927       4
#> 63  119     1   left  3082  3296      214       5
#> 64  119     1 centre  3319  4162      843       6
#> 65  119     1   left  4195  4655      460       7
#> 66  119     1  right  4709  5005      296       8
#> 67  119     1   left  5155  7305     2150       9
#> 68  119     1  right  7361  7888      527      10
#> 69  119     1 centre  7931  8448      517      11
#> 70  119     1 centre  8894  9111      217      12
#> 71  119     1   left  9148  9608      460      13
#> 72  119     1 centre  9631  9988      357      14
#> 73  119     2   left   190   566      376       1
#> 74  119     2  right   620   953      333       2
#> 75  119     2 centre  1263  2936     1673       3
#> 76  119     2   left  3243  3566      323       4
#> 77  119     2 centre  3649  3996      347       5
#> 78  119     2   left  4249  4462      213       6
#> 79  119     2  right  4516  5066      550       7
#> 80  119     3   left   213   670      457       1
#> 81  119     3  right   720  1163      443       2
#> 82  119     3 centre  1193  2016      823       3
#> 83  119     3  right  2403  3116      713       4
#> 84  119     3   left  3170  3929      759       5
#> 85  119     3  right  3983  4989     1006       6
#> 86  119     3   left  5109  5689      580       7
#> 87  119     3 centre  5736  7135     1399       8
#> 88  119     3   left  7402  7569      167       9
#> 89  119     4   left   263  1390     1127       1
#> 90  119     4  right  1436  1856      420       2
#> 91  119     4   left  1946  2996     1050       3
#> 92  119     4 centre  3026  3259      233       4
#> 93  119     4   left  3289  4143      854       5
#> 94  119     4  right  4196  5419     1223       6
#> 95  119     4   left  5472  6066      594       7
#> 96  119     4  right  6225  6625      400       8
#> 97  119     4   left  6675  7359      684       9
#> 98  119     4 centre  7545  9052     1507      10
#> 99  119     5   left   240  1100      860       1
#> 100 119     5  right  1153  2723     1570       2
#> 101 119     5   left  2916  3390      474       3
#> 102 119     5  right  3446  4489     1043       4
#> 103 119     5   left  4563  5519      956       5
#> 104 119     5  right  5583  6396      813       6
#> 105 119     5   left  6699  7395      696       7
#> 106 119     5 centre  7425  9009     1584       8
#> 107 119     5   left  9065  9565      500       9
#> 108 119     5 centre  9595 10432      837      10
#> 109 119     5   left 10612 11751     1139      11
#> 110 119     5 centre 11791 12378      587      12
#> 111 119     5   left 12624 12834      210      13
#> 112 119     6 centre   440   593      153       1
#> 113 119     6   left   626  3053     2427       2
#> 114 119     6  right  3189  4092      903       3
#> 115 119     6 centre  4256  6778     2522       4
#> 116 119     6  right  6888  7558      670       5
#> 117 119     6 centre  7595  9365     1770       6
#> 118 119     6  right  9421 10288      867       7
#> 119 119     6   left 10338 11021      683       8
#> 120 119     6  right 11074 11584      510       9
#> 121 119     6   left 11644 11997      353      10
#> 122 119     6 centre 12064 12671      607      11
# }
```
