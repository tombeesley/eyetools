# Example dataset from that contains binocular eye data from two participants from a simple contingency learning task (the data are from Beesley, Nguyen, Pearson, & Le Pelley, 2015). In this task there are two stimuli that appear simultaneously on each trial (to the left and right of the screen). Participants look at these cues and then make a decision by selecting an "outcome response" button. Data was recorded with a Tobii TX300 sampling at 300Hz. Participant sat approximately 60 cm from the screen. The screen resolution was 1920 x 1080.

The dataset contains data from two participants and the first six trials
of the study.

## Usage

``` r
HCL
```

## Format

A dataframe of 31,041 observations and seven variables

- pID:

  participant ID

- time:

  timestamp of the sample (milliseconds)

- left_x:

  x coordinate of the left eye

- left_y:

  y coordinate of the left eye

- right_x:

  x coordinate of the right eye

- right_y:

  y coordinate of the right eye

- trial:

  trial number
