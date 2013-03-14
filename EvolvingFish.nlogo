breed [fishes fish]
breed [sharks shark]

globals [
  fish-energy-threshold
  shark-energy-threshold
]

turtles-own [
  flockmates         ;; agentset of nearby turtles
  nearest-neighbor   ;; closest one of our flockmates
  energy
  ;; Staying close:
  max-align-turn
  max-align-speed
  max-cohere-turn
  max-cohere-speed
  max-separate-turn
  max-separate-speed
  max-food-turn ;; Find food
  food-speed-slope
  speed-weights
  
  max-gene-turn
]

fishes-own [
  sharks-nearby      ;; agentset of nearby sharks
  max-flee-turn      ;; Avoid predators
  flee-speed-slope
]

sharks-own [
  fishes-nearby      ;; agentset of nearby fishes
]

patches-own [
  well      ;; the amount of resources a patch has
  is-well?
]

to setup
  clear-all
  set fish-energy-threshold 25
  set shark-energy-threshold 100

  create-fishes fish-population
    [ set color red - 2 + random 4  ;; random shades look nice
      set size 1.5  ;; easier to see
      setxy random-xcor random-ycor
      set shape "fish"
      
      set energy 20
      set max-food-turn random-float 10
      set flee-speed-slope random-float 10
      set max-align-turn random-float 10
      set max-cohere-turn random-float 10
      set max-separate-turn random-float 10
      set max-flee-turn random-float 10 
 
      set max-gene-turn 10
      ;; set max-speed
    ]
  create-sharks shark-population
    [ set color gray - 2 + random 4  ;; random shades look nice
      set size 4  ;; easier to see
      setxy random-xcor random-ycor
      set shape "shark"
      
      set energy shark-energy-threshold * 2 - 1
      set max-food-turn random-float 5
      set food-speed-slope random-float 5
      set max-align-turn random-float 5
      set max-cohere-turn random-float 5
      set max-separate-turn random-float 5
      
      set max-gene-turn 5
      ]
  
  setup-patches

  reset-ticks
end

to setup-patches ;; Make sure food is plenty :-)
  ask patches [

    ifelse (random-float 1 < food-density / 1000.0 )
    [ set is-well? true 
      set well 1]
    [ set is-well? false ]
    recolor-patch
  ]
end

to go
  ask patches [ replenish ]
  ask turtles [ flock ] 
  ask turtles with [ energy <= 0 ] [ die ]
  ask sharks [ set speed-weights [0.4] hunt eat-fish ]
  ask sharks with [ energy > shark-energy-threshold ] [ reproduce-shark ]
  ask fishes [  set speed-weights [0.2] flee find-food eat-patch ]
  ask fishes with [ energy > fish-energy-threshold ] [ reproduce-fish ]
  ask patches [ recolor-patch ]


  ask turtles [ 
    let weight mean speed-weights
    fd 1 * weight 
    set energy energy - weight * move-cost]
  tick
end

;;;;; Patch procedures 

to replenish  ;; replenishes and moves food to neighboring patches.
  if (well < max-well and is-well?) 
    [set well well + replenish-speed]
  if (well > max-well * well-spread / 100) [ ;; When to spread food
    let infect one-of neighbors
    let overflow well - (max-well * well-spread / 100)
    let difference well - [well] of infect
    let change min list overflow difference / 5
    set well well - change
    ask infect [set well well + change]
  ]
end

to recolor-patch  ;; patch procedure
   set pcolor scale-color green well 0 max-well
end


to flock  ;; turtle procedure
  find-flockmates
  if any? flockmates
    [ find-nearest-neighbor
      ifelse distance nearest-neighbor < minimum-separation
        [ separate ]
        [ align
          cohere ] ]
end

to find-flockmates  ;; turtle procedure
  set flockmates other breed in-radius vision
end

to find-nearest-neighbor ;; turtle procedure
  set nearest-neighbor min-one-of flockmates [distance myself]
end

;;; SEPARATE

to separate  ;; turtle procedure
  turn-away ([heading] of nearest-neighbor) max-separate-turn
end

;;; ALIGN

to align  ;; turtle procedure
  turn-towards average-flockmate-heading max-align-turn
end

to-report average-flockmate-heading  ;; turtle procedure
  ;; We can't just average the heading variables here.
  ;; For example, the average of 1 and 359 should be 0,
  ;; not 180.  So we have to use trigonometry.
  let x-component sum [dx] of flockmates
  let y-component sum [dy] of flockmates
  ifelse x-component = 0 and y-component = 0
    [ report heading ]
    [ report atan x-component y-component ]
end

;;; COHERE

to cohere  ;; turtle procedure
  turn-towards average-heading-towards-flockmates max-cohere-turn
end

to-report average-heading-towards-flockmates  ;; turtle procedure
  ;; "towards myself" gives us the heading from the other turtle
  ;; to me, but we want the heading from me to the other turtle,
  ;; so we add 180
  let x-component mean [sin (towards myself + 180)] of flockmates
  let y-component mean [cos (towards myself + 180)] of flockmates
  ifelse x-component = 0 and y-component = 0
    [ report heading ]
    [ report atan x-component y-component ]
end

;;; SHARK PROCEDURES

to find-fishes
  set fishes-nearby other fishes in-radius vision
end

to hunt
  find-fishes
  if any? fishes-nearby
    [ turn-towards average-heading-towards-fishes max-food-turn
      set speed-weights 
          fput calculate-weight food-speed-slope (mean [distance myself] of fishes-nearby) 
               speed-weights ]
end

to eat-fish
  set energy energy + ((sum [energy] of fishes-here) * entropy / 100)
  ask fishes-here [die]
end

to reproduce-shark  
  let candidates sharks-here with [self > myself]
  if any? candidates [ mate (turtle-set self one-of candidates) shark-energy-threshold]
  if energy >= shark-energy-threshold * 2 [mate (turtle-set self self) shark-energy-threshold]
end

to-report average-heading-towards-fishes  ;; turtle procedure
  ;; "towards myself" gives us the heading from the other turtle
  ;; to me, but we want the heading from me to the other turtle,
  ;; so we add 180
  let x-component mean [sin (towards myself + 180)] of fishes-nearby
  let y-component mean [cos (towards myself + 180)] of fishes-nearby
  ifelse x-component = 0 and y-component = 0
    [ report heading ]
    [ report atan x-component y-component ]
end

;;; FISH PROCEDURES

to find-sharks
  set sharks-nearby other sharks in-radius vision
end

to flee
  find-sharks
  if any? sharks-nearby
    [ turn-away average-heading-towards-sharks max-flee-turn 
      set speed-weights 
          fput calculate-weight flee-speed-slope (mean [distance myself] of sharks-nearby) 
               speed-weights ]
end

to eat-patch
   if (energy < fish-energy-threshold * 2) and (well > 0) [
    set energy energy + ( well / count fishes-here )
    set well well - ( well / count fishes-here )
  ]
end 

;; if there are any other agents at your location, reproduce with them
;; We compare id numbers to prevent the same pair from reproducing twice
to reproduce-fish  
  let candidates fishes-here with [self > myself]
  if any? candidates [ mate (turtle-set self one-of candidates) fish-energy-threshold]
end

to-report average-heading-towards-sharks  ;; turtle procedure
  ;; "towards myself" gives us the heading from the other turtle
  ;; to me, but we want the heading from me to the other turtle,
  ;; so we add 180
  let x-component mean [sin (towards myself + 180)] of sharks-nearby
  let y-component mean [cos (towards myself + 180)] of sharks-nearby
  ifelse x-component = 0 and y-component = 0
    [ report heading ]
    [ report atan x-component y-component ]
end

to find-food
  let food max-one-of other patches in-radius vision [well]
  let x-component sin (towards food)
  let y-component cos (towards food)
  let a heading
  if x-component != 0 or y-component != 0
    [ set a atan x-component y-component ]
    
  turn-towards a max-food-turn
end

;;; HELPER PROCEDURES

to-report calculate-weight [slope value]
  let normalized-slope (slope - max-gene-turn / 2) / max-gene-turn
  report normalized-slope * (normalize-vision value) / 2 + 0.5
end

to-report normalize-vision [value]
  report value / vision
end

to turn-towards [new-heading max-turn]  ;; turtle procedure
  turn-at-most (subtract-headings new-heading heading) max-turn
end

to turn-away [new-heading max-turn]  ;; turtle procedure
  turn-at-most (subtract-headings heading new-heading) max-turn
end

;; Creates offspring from mating
to mate [agents threshold]
  if all? agents [energy > threshold]
    [
      hatch 1 [

        set max-align-turn    combine-gene [max-align-turn]    of agents
        set max-cohere-turn   combine-gene [max-cohere-turn]   of agents
        set max-separate-turn combine-gene [max-separate-turn] of agents
        set max-food-turn     combine-gene [max-food-turn]     of agents
        set food-speed-slope  combine-gene [food-speed-slope]  of agents
        if all? agents [ is-fish? self ]
        [ set max-flee-turn    combine-gene [max-flee-turn]     of agents
          set flee-speed-slope combine-gene [flee-speed-slope]  of agents ]

        
        setxy xcor + random 2 ycor - random 2
        set energy threshold
      ]
      ask agents [ set energy energy - threshold / 2 ]
    ]
end

to-report combine-gene [genes]
  report mutate one-of genes
end

;; turn right by "turn" degrees (or left if "turn" is negative),
;; but never turn more than "max-turn" degrees
to turn-at-most [turn max-turn]  ;; turtle procedure
  ifelse abs turn > max-turn
    [ ifelse turn > 0
        [ rt max-turn ]
        [ lt max-turn ] ]
    [ rt turn ]
end

to-report mutate [value]
  let new-value (value + ((random-float 2 * mutation-step) - mutation-step))
  let min-turn 0
  report ifelse-value (random-float 100 < mutation-rate)
    [ min (list max-gene-turn (max (list min-turn new-value))) ]
    [ value ]
end

; Copyright 1998 Uri Wilensky.
; See Info tab for full copyright and license.
@#$#@#$#@
GRAPHICS-WINDOW
250
10
965
746
70
70
5.0
1
10
1
1
1
0
1
1
1
-70
70
-70
70
1
1
1
ticks
30.0

BUTTON
39
93
116
126
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
122
93
203
126
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
9
51
232
84
fish-population
fish-population
1.0
1000.0
246
1.0
1
NIL
HORIZONTAL

SLIDER
9
135
232
168
vision
vision
0.0
10.0
6
0.5
1
patches
HORIZONTAL

SLIDER
9
169
232
202
minimum-separation
minimum-separation
0.0
5.0
1
0.25
1
patches
HORIZONTAL

SLIDER
9
14
232
47
shark-population
shark-population
0
100
15
1
1
NIL
HORIZONTAL

PLOT
1203
21
1403
171
Fishes
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count fishes"

PLOT
1203
179
1403
329
Sharks
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count sharks"

PLOT
1408
20
1608
170
Fish energy
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"mean" 1.0 0 -16777216 true "" "plot mean [energy] of fishes"
"min" 1.0 0 -7500403 true "" "plot min [energy] of fishes"
"max" 1.0 0 -2674135 true "" "plot max [energy] of fishes"

PLOT
1408
178
1608
328
Shark energy
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"mean" 1.0 0 -16777216 true "" "plot mean [energy] of sharks"
"min" 1.0 0 -7500403 true "" "plot min [energy] of sharks"
"max" 1.0 0 -2674135 true "" "plot max [energy] of sharks"

SLIDER
21
276
199
309
replenish-speed
replenish-speed
0
20
0.6
0.2
1
NIL
HORIZONTAL

PLOT
1203
339
1403
489
Algae
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot sum [well] of patches"

PLOT
1204
525
1486
698
Fishes mean turns
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"flee" 1.0 0 -16777216 true "" "plot mean [max-flee-turn] of fishes"
"food" 1.0 0 -7500403 true "" "plot mean [max-food-turn] of fishes"
"align" 1.0 0 -2674135 true "" "plot mean [max-align-turn] of fishes"
"cohere" 1.0 0 -955883 true "" "plot mean [max-cohere-turn] of fishes"
"separate" 1.0 0 -6459832 true "" "plot mean [max-separate-turn] of fishes"
"flee-slope" 1.0 0 -1184463 true "" "plot mean [flee-speed-slope] of fishes"

PLOT
1414
338
1683
502
Sharks mean turns
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"food" 1.0 0 -7500403 true "" "plot mean [max-food-turn] of sharks"
"align" 1.0 0 -2674135 true "" "plot mean [max-align-turn] of sharks"
"cohere" 1.0 0 -955883 true "" "plot mean [max-cohere-turn] of sharks"
"separate" 1.0 0 -6459832 true "" "plot mean [max-separate-turn] of sharks"
"food-slope" 1.0 0 -1184463 true "" "plot mean [food-speed-slope] of sharks"

SLIDER
20
316
192
349
food-density
food-density
0
100
5
1
1
NIL
HORIZONTAL

SLIDER
23
416
195
449
mutation-rate
mutation-rate
0
100
50
1
1
NIL
HORIZONTAL

SLIDER
23
455
195
488
mutation-step
mutation-step
0
100
4
1
1
NIL
HORIZONTAL

SLIDER
20
236
192
269
max-well
max-well
0
100
14
1
1
NIL
HORIZONTAL

SLIDER
20
357
192
390
well-spread
well-spread
0
100
65
1
1
NIL
HORIZONTAL

SLIDER
22
498
194
531
move-cost
move-cost
0
2
0.5
0.05
1
NIL
HORIZONTAL

SLIDER
23
551
195
584
entropy
entropy
0
100
50
1
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

This model is an attempt to mimic the flocking of birds.  (The resulting motion also resembles schools of fish.)  The flocks that appear in this model are not created or led in any way by special leader birds.  Rather, each bird is following exactly the same set of rules, from which flocks emerge.

## HOW IT WORKS

The birds follow three rules: "alignment", "separation", and "cohesion".

"Alignment" means that a bird tends to turn so that it is moving in the same direction that nearby birds are moving.

"Separation" means that a bird will turn to avoid another bird which gets too close.

"Cohesion" means that a bird will move towards other nearby birds (unless another bird is too close).

When two birds are too close, the "separation" rule overrides the other two, which are deactivated until the minimum separation is achieved.

The three rules affect only the bird's heading.  Each bird always moves forward at the same constant speed.

## HOW TO USE IT

First, determine the number of birds you want in the simulation and set the POPULATION slider to that value.  Press SETUP to create the birds, and press GO to have them start flying around.

The default settings for the sliders will produce reasonably good flocking behavior.  However, you can play with them to get variations:

Three TURN-ANGLE sliders control the maximum angle a bird can turn as a result of each rule.

VISION is the distance that each bird can see 360 degrees around it.

## THINGS TO NOTICE

Central to the model is the observation that flocks form without a leader.

There are no random numbers used in this model, except to position the birds initially.  The fluid, lifelike behavior of the birds is produced entirely by deterministic rules.

Also, notice that each flock is dynamic.  A flock, once together, is not guaranteed to keep all of its members.  Why do you think this is?

After running the model for a while, all of the birds have approximately the same heading.  Why?

Sometimes a bird breaks away from its flock.  How does this happen?  You may need to slow down the model or run it step by step in order to observe this phenomenon.

## THINGS TO TRY

Play with the sliders to see if you can get tighter flocks, looser flocks, fewer flocks, more flocks, more or less splitting and joining of flocks, more or less rearranging of birds within flocks, etc.

You can turn off a rule entirely by setting that rule's angle slider to zero.  Is one rule by itself enough to produce at least some flocking?  What about two rules?  What's missing from the resulting behavior when you leave out each rule?

Will running the model for a long time produce a static flock?  Or will the birds never settle down to an unchanging formation?  Remember, there are no random numbers used in this model.

## EXTENDING THE MODEL

Currently the birds can "see" all around them.  What happens if birds can only see in front of them?  The `in-cone` primitive can be used for this.

Is there some way to get V-shaped flocks, like migrating geese?

What happens if you put walls around the edges of the world that the birds can't fly into?

Can you get the birds to fly around obstacles in the middle of the world?

What would happen if you gave the birds different velocities?  For example, you could make birds that are not near other birds fly faster to catch up to the flock.  Or, you could simulate the diminished air resistance that birds experience when flying together by making them fly faster when in a group.

Are there other interesting ways you can make the birds different from each other?  There could be random variation in the population, or you could have distinct "species" of bird.

## NETLOGO FEATURES

Notice the need for the `subtract-headings` primitive and special procedure for averaging groups of headings.  Just subtracting the numbers, or averaging the numbers, doesn't give you the results you'd expect, because of the discontinuity where headings wrap back to 0 once they reach 360.

## RELATED MODELS

* Moths
* Flocking Vee Formation

## CREDITS AND REFERENCES

This model is inspired by the Boids simulation invented by Craig Reynolds.  The algorithm we use here is roughly similar to the original Boids algorithm, but it is not the same.  The exact details of the algorithm tend not to matter very much -- as long as you have alignment, separation, and cohesion, you will usually get flocking behavior resembling that produced by Reynolds' original model.  Information on Boids is available at http://www.red3d.com/cwr/boids/.


## HOW TO CITE

If you mention this model in a publication, we ask that you include these citations for the model itself and for the NetLogo software:

* Wilensky, U. (1998).  NetLogo Flocking model.  http://ccl.northwestern.edu/netlogo/models/Flocking.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.
* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## COPYRIGHT AND LICENSE

Copyright 1998 Uri Wilensky.

![CC BY-NC-SA 3.0](http://i.creativecommons.org/l/by-nc-sa/3.0/88x31.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit http://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Commercial licenses are also available. To inquire about commercial licenses, please contact Uri Wilensky at uri@northwestern.edu.

This model was created as part of the project: CONNECTED MATHEMATICS: MAKING SENSE OF COMPLEX PHENOMENA THROUGH BUILDING OBJECT-BASED PARALLEL MODELS (OBPML).  The project gratefully acknowledges the support of the National Science Foundation (Applications of Advanced Technologies Program) -- grant numbers RED #9552950 and REC #9632612.

This model was converted to NetLogo as part of the projects: PARTICIPATORY SIMULATIONS: NETWORK-BASED DESIGN FOR SYSTEMS LEARNING IN CLASSROOMS and/or INTEGRATED SIMULATION AND MODELING ENVIRONMENT. The project gratefully acknowledges the support of the National Science Foundation (REPP & ROLE programs) -- grant numbers REC #9814682 and REC-0126227. Converted from StarLogoT to NetLogo, 2002.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
true
0
Polygon -1 true false 131 256 87 279 86 285 120 300 150 285 180 300 214 287 212 280 166 255
Polygon -1 true false 195 165 235 181 218 205 210 224 204 254 165 240
Polygon -1 true false 45 225 77 217 103 229 114 214 78 134 60 165
Polygon -7500403 true true 136 270 77 149 81 74 119 20 146 8 160 8 170 13 195 30 210 105 212 149 166 270
Circle -16777216 true false 106 55 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

shark
true
0
Polygon -7500403 true true 153 17 149 12 146 29 145 -1 138 0 119 53 107 110 117 196 133 246 134 261 99 290 112 291 142 281 175 291 185 290 158 260 154 231 164 236 161 220 156 214 160 168 164 91
Polygon -7500403 true true 161 101 166 148 164 163 154 131
Polygon -7500403 true true 108 112 83 128 74 140 76 144 97 141 112 147
Circle -16777216 true false 129 32 12
Line -16777216 false 134 78 150 78
Line -16777216 false 134 83 150 83
Line -16777216 false 134 88 150 88
Polygon -7500403 true true 125 222 118 238 130 237
Polygon -7500403 true true 157 179 161 195 156 199 152 194

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.0.3
@#$#@#$#@
set population 200
setup
repeat 200 [ go ]
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 1.0 0.0
0.0 1 1.0 0.0
0.2 0 1.0 0.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
