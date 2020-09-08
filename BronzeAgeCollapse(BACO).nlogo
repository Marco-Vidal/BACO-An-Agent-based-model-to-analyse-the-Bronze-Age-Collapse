    ;;;;;;;;;;;; BRONZE AGE COLLAPASE MODEL;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                ;;;;;;;;;;;;; GENERAL PUBLIC LICENSE  (GPL);;;;;;;;;;;;;
;; Copyright (C) 2020  Marco Vidal-Cordasco (marcovidalcordasco@gmail.com)

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation , either version 3 of the License, or
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see https://www.gnu.org/licenses/gpl-3.0.txt

;; Permission to use, modify or redistribute this model is hereby granted,
;; provided that this copyright notice is included and
;; the model is not redistributed for profit.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


extensions [ gis profiler ]

breed [oikoi oikos] ; oikoi = households

breed [settlements settlement] ; settlements = cities

breed [seapeoples seapeople] ; Sea-peoples = raiders

breed [halos halo] ; just for optional visual representation purposes

patches-own
[
  elevation ; real terrain elevation in meters
  slope ; terrain slope expresed in degrees
  slope-factor ; factor reflecting the terrain slope influence on the yield productivity
  dif-elevation ; elevation difference between adjacent cells
  cost-slope-P ; energetic cost of walking 1 km through certain cell when the slope is positive
  cost-slope-N ; energetic cost of walking 1 km through certin cell when the slope is negative
  soil-capacity ; maximum yield productivity of the patch
  soil-fertility ; quality of the cell
  initial-soil-fertility
  rainfall ; rainfall signal based on the Rainfall Anomaly Index
  oikoi-per-patch ; number of households per patch
  expected-yield ; Expected yield productivity
  safe? ; patches without raiders nearby are safe
]

settlements-own
[
  n-settlements ; number of settlements
  defense-capability ; total defence capability of settlements against raids
  deffense-from-oikoi_surplus ; defence capability of settlements against raids on the basis of their oikoi_surplus
  deffense-from-population-size ; defence capability of settlements against raids on the basis of their population density
  deffense-from-orography  ; defence capability of settlements on the basis of the terrain slope
  settlement-elevation ; terrain elevation in meters
  destroyed? ; settlements may be destroyed by raiders
  attacked? ; settlements may be attacked by raiders
]

oikoi-own
[
  ; Diet and Energy:

  oikoi_energy-requirements ; kilocalories needed to survive one year per household
  R-sea ; total percentage of marine resources included in the diet
  R-agriculture ; percentage of resources obtained from farming included in the diet
  ; Farming:

  farming-assets ; it improves the yield productivity
  extension-cultivated ; land extension devoted to farming
  oikoi_stocks ; energy stored
  oikoi_harvest ; agriculture output expressed in kcal
  oikoi_shortage ; energetic deficit
  oikoi_surplus ; energetic excedent once storage limit is reached
  ; Exchanges:

  loan ; amount of energy that households with energetic oikoi_shortage request to their neighbors
  debt ; amount of energy given to certain household with oikoi_shortage
  safe-route? ; trade route with no raiders
  customer-distance ; distance between two households which perform commercial transactions
  net-oikoi_surplus ; energetic oikoi_surplus minus the energetic costs of trading
  ; Migrations

  emigrate? ; households emigrate because of raids or when the energy obtained from oikoi_harvest is not enough to meet the energetic requirements
  residence-continuity ; number of years that a household has been on certain patch
  my-city ; settlement that provides protection to the household
  no-city ; households without settlement's protection
  target-patch ; patches with a better expected yield productivity
 ]

seapeoples-own
[
  my-halo
  city-target-patch
]
halos-own
[
  my-owner
]

globals
[
  ; Environment variables and representations:

  elevation-dataset ; basemap
  real-size-km ; real size of each cell expressed in kilometers
  min-elevation ; min. terrain elevation in meters
  max-elevation ; max. terrain elevation in meters
  RAI ; Rainfall Anomaly Index
  cRAI ; Counter of the Rainfall Anomaly Index
  land-patches ; land
  sea-patches ; sea
  coastal-patches ; patches that are close (< 20 km) from the coast
  inland-patches ; patches that are far away (> 20 km) from the shore
  ; Migrations and timer

  n-migrations ; number of migrations
  calendar ; year BC
  ; Settlements and households:

  abandoned-coastal-cities ; number of abandoned coastal settlements
  abandoned-inland-cities ; number of abandoned inland settlements
  new-coastal-cities
  new-inland-cities
  settlement-perimeter ; influencing area of each settlement. It is assumed to be a constant.
  coastal-area ;patches near (<20km) the coast. It is assumed to be a constant.
  maxdebt ; max energy that can be exchanged among neighbours
  storagelimit ; maximum storage capacity expressed in kcal/year
  trade-flows ; number of commercial exchanges
  max-cultivable-extension ; max. extension of cultivable land per patch in hectares. It is assumed to be a constant.
  seed ; used to control the random number generator seed
]

                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                        ;;;;;;;; MODEL SETUP ;;;;;;;;
                        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup

  ca
  random-seed seed

  import-RAI
  import-basemap

  set-global-varialbles
  set-initial-soil-attributes

  create-settlements&populations
  set-initial-view

end


to import-RAI

  ifelse ( file-exists? "RAI.txt" ) ; before setup, the "RAI.txt" file must be located in the same folder as the model itself
  [
    set RAI []
    file-open "RAI.txt"
    while [not file-at-end?]
    [set RAI sentence RAI (list file-read)]
    file-close
  ]
  [
    user-message "Please save the RAI.txt file in the same folder as the model itself"
  ]
end



to import-basemap

set elevation-dataset gis:load-dataset "BASEMAP.asc" ; before setup, the "BASEMAP.asc" file must be located in the same folder as the model itself
let resolution cell-size / 2 ; It can be asjusted by the user by using the "Cell-Size" buttom
resize-world 0 (( gis:width-of elevation-dataset - 1 ) / resolution ) 0 (( gis:height-of elevation-dataset - 1 ) / resolution )
set-patch-size ( 2 * Cell-Size )
gis:set-world-envelope gis:envelope-of elevation-dataset
gis:apply-raster elevation-dataset elevation
gis:set-sampling-method elevation-dataset "BICUBIC_2"
set min-elevation gis:minimum-of elevation-dataset
set max-elevation gis:maximum-of elevation-dataset

  ask patches
  [
    ifelse ( elevation <= 0 ) or ( elevation > 0 )
    [set elevation elevation]
    [set elevation 0]
  ]

 set sea-patches patches with [elevation <= 0]
 set land-patches patches with [elevation > 0]
 set real-size-km cell-size * 2.1 ; If the Cell-Size is 1, the real extension of one patch is 2.1 km. It is automatically adjusted when the user changes the map resolution

  ask land-patches
  [
    let D 0
    ask neighbors with [elevation != [elevation] of myself]
    [
      let dif abs (elevation - [elevation] of myself)
      if (dif > D)
      [
        let m (dif / (real-size-km * 1000)) ; dif is expressed in meters, so Real-Size-km is also converted to meters.
        set slope precision asin (m / sqrt(1 + m * m)) 2 ; slope is expressed in degrees
        ifelse (elevation > [elevation] of myself)
              [set dif-elevation 1]
              [set dif-elevation -1]
     ]
    ]

  set slope-factor precision ((1 - 0.06)^ slope) 2
  set cost-slope-P (exp(4.65177 + 0.0219174 * (slope * dif-elevation)))
  set cost-slope-N (exp(4.65177 + 0.0219174 * (slope * (dif-elevation * -1))))
  ]
end



 to set-global-varialbles

  set abandoned-coastal-cities 0
  set abandoned-inland-cities 0
  set new-coastal-cities 0
  set new-inland-cities 0
  set calendar 1350 ; B.C.
  set cRAI 0 ; RAI counter
  set-constants

end

to set-constants
  set max-cultivable-extension (real-size-km * 100) * 0.8 ; it is assmed that a maximum of 80% of the hectares of each land-patch can be cultivated
  set settlement-perimeter round (25 / Real-Size-km) ; each settlement has a perimeter of 25 km^2
  set coastal-area 20 / Real-Size-km
  set coastal-patches land-patches with [any? sea-patches in-radius coastal-area]
  set inland-patches land-patches with [not any? sea-patches in-radius coastal-area]
  set maxdebt max-debt * 3700000
end



  to set-initial-soil-attributes

  ;;;;;;;;;;; INITIAL SOIL-CAPACITY

  ask patches
  [
    set soil-capacity random-normal 1600 300 ; the maximum soil productivity (productivity-capacity) is expressed in kg/ha/year
  ]
  repeat 2
  [ diffuse soil-capacity 1 ]

  ;;;;;;;;;;; INITIAL SOIL-FERTILITY

  ask land-patches
  [
    set initial-soil-fertility (( 0.65 + random-float 0.34)  * soil-capacity) set Safe? true
  ]

end



to create-settlements&populations

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SETTLEMENTS

  create-settlements 40
  [
  set shape "house"
  set size (5 / cell-size)
  set color red + 1
  set attacked? false
  set destroyed? false
  set deffense-from-oikoi_surplus 1
  set deffense-from-population-size 1
  ifelse ([who] of self <= 20)
    [move-to one-of coastal-patches] ; Initially, 50% of the settlements are inland-settlements and the other 50% coastal-settlements
    [move-to one-of inland-patches]
  ]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; POPULATIONS

  create-oikoi number-oikoi
  [
    set size (2 / cell-size)
    set color white
    move-to one-of settlements rt random-float 360 fd random-float 2
    if ([pcolor] of patch-here = blue)
    [move-to one-of land-patches]

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; DIET & ENERGY REQUIREMENTS
    let sea-distance min-one-of sea-patches [distance myself]
    set R-sea (marine-food-resources - (0.01 * distance sea-distance))
    ifelse (R-sea + other-calorie-sources < 0.8)
    [
      set R-agriculture (1 - (R-sea + other-calorie-sources))
    ]
    [
      set R-agriculture 0.2
    ]

    set oikoi_energy-requirements 7300000 * R-agriculture ; the energetic requirement per person is 730,000 kcal/year; per household 7,300,000 kcal/year
    set oikoi_harvest oikoi_energy-requirements * 2 ; the initial harves is twice the energetic requirements
    set oikoi_surplus 0
    set extension-cultivated 2 ; in hectares
    set emigrate? false
    set Safe-route? true

    set my-city min-one-of (settlements) [distance myself]
    ifelse (distance my-city) < settlement-perimeter [create-link-with my-city set no-city false] [set no-city true]

    set oikoi-per-patch count oikoi-here
    set oikoi_stocks 0
    set debt 0
    set residence-continuity 1
  ]
end



  to set-initial-view

 if (view = "Elevation" or view = "RAI")
  [
    ask sea-patches [set pcolor blue] ; "RAI" view needs to update the cRAI and RAI data each step, so initially is used the "Elevation" view. Once the model is run, the view will be updated with RAI data whenever "Update-View" = true.
    ask land-patches [set pcolor scale-color green elevation min-elevation max-elevation]
  ]

 if (view = "soil-capacity")
  [
    let min-capacity min [soil-capacity] of land-patches
    let max-capacity max [soil-capacity] of land-patches
    ask sea-patches [set pcolor blue]
    ask land-patches
    [
      set pcolor scale-color green soil-capacity min-capacity max-capacity
    ]
  ]
end

                                           ;;;;;;;;;;;;;;;;;;;;;;;
                                           ;;;;;;PROCEDURE;;;;;;;;
                                           ;;;;;;;;;;;;;;;;;;;;;;;

to go

  ;;;;;; COUNTER & RAINFALL ANOMALY INDEX (RAI)  ;;;;;;

 set calendar calendar - 1

 ifelse (cRAI = 430)
  [set cRAI 0]
  [set cRAI cRAI + 1]

 if (calendar = 920) [stop]

 if (not any? oikoi)
  [user-message "Civilization demise" stop]

  ;;;;;;  oikoi_harvest, oikoi_stocks & oikoi_surplus  ;;;;;;

  calculate-soil-productivity
  oikoi_harvesting
  calculate-oikoi_shortage&oikoi_surplus

  ;;;;;; COMMERCE & EMIGRATION  ;;;;;;

  exchanges-among-neighbors?
  if activate-commerce? [simulate-external-exchanges]
  simulate-migrations

  ;;;;;; CITY & POPULATION DYNAMICS  ;;;;;;

  simulate-city-dynamics
  simulate-population-dynamics

  ;;;;;; SEA PEOPLES  ;;;;;;

  if sea-people? and calendar = beginning-raids
  [
    create-seapeoples number-sea-people
    [
    set shape "boat"
    set size (5.5 / cell-size)
    set color black
    setxy random-xcor random-ycor
    if ([pcolor] of patch-here != blue)
      [move-to one-of sea-patches]
    if (halo?)
      [make-halo]
    ]
  ]

   if (calendar = end-raids)
  [
    ask seapeoples [die]
    ask settlements with [attacked? = true]
    [set attacked? false]
    ask settlements with [destroyed? = true]
    [set destroyed? false]
    ask patches with [Safe? = false]
    [set safe? true]
   ]

  ifelse (any? seapeoples)
  [ask seapeoples [raiding]]
  [ask halos [die]]

  if (halo? = false and any? halos)
  [ask halos [die]]

  if (halo? = true and not any? halos)
  [ask seapeoples [make-halo]]

  ;;;;;; VIEW & PLOTS  ;;;;;;

  do-plots

if update-view
  [
    if (view = "RAI")
    [
      ask sea-patches
      [set pcolor blue]
      ask land-patches
      [
        set pcolor scale-color orange rainfall 4 -4
      ]
    ]

    if (view = "soil-capacity")
    [
      let min-capacity min [soil-capacity] of land-patches
      let max-capacity max [soil-capacity] of land-patches
      ask sea-patches [set pcolor blue]
      ask land-patches
      [
        set pcolor scale-color green soil-capacity min-capacity max-capacity
      ]
    ]

   if (view = "Elevation")
    [
      ask sea-patches [set pcolor blue]
      ask land-patches
      [
        set pcolor scale-color green elevation min-elevation max-elevation
      ]
    ]
   ]

end


  ;;;;;;  oikoi_harvest, STOCK & oikoi_surplus  ;;;;;;

to calculate-soil-productivity

  ask land-patches
  [
    set oikoi-per-patch count oikoi-here

    let soil-depletion depletion-rate * oikoi-per-patch
    let soil-recovery soil-fertility * ((soil-fertility / soil-capacity) ^ degradation-rate)
    * (1 - soil-fertility / soil-capacity) * recovery-rate

    set soil-fertility initial-soil-fertility + soil-recovery - soil-depletion


    if (soil-fertility < 0) [set soil-fertility 0.1]

    set expected-yield (soil-fertility + recovery-rate * soil-fertility * ((soil-fertility / soil-capacity) ^ degradation-rate)
    * (1 - soil-fertility / soil-capacity) - depletion-rate * (count oikoi-here + 1))  * slope-factor

    if (expected-yield < 0) [set expected-yield 0.1]

    set rainfall 1.5 * (1 - exp ( (ln(1 / 3) / 4)  * (item cRAI RAI + 4)))

    set rainfall rainfall * (1 + random-normal 0 rainfall-variability)
  ]

end


to oikoi_harvesting

  let max-farming-assets-improvement 2.5
  ask land-patches
  [
  let total-residence-continuity 0
  ask oikoi-here
    [
      set total-residence-continuity total-residence-continuity + residence-continuity
      ifelse (total-residence-continuity = 0)
      [
        set farming-assets 1 / (1 + farming-assets-factor)
      ]
      [
        set farming-assets (total-residence-continuity / (total-residence-continuity + (Max-farming-assets-improvement - farming-assets-factor)))
      ]
     ]
  ]

  ask oikoi
  [
    set oikoi_harvest (([rainfall] of patch-here * [slope-factor] of patch-here * farming-assets *
    [soil-fertility] of patch-here)* extension-cultivated * 3390)

    if (extension-cultivated < max-extension and oikoi_harvest < oikoi_energy-requirements)
    [
      set extension-cultivated extension-cultivated + (1 - (sum [extension-cultivated] of oikoi-here / max-cultivable-extension))
    ]
  ]
end



to  calculate-oikoi_shortage&oikoi_surplus

set storagelimit storage-limit * 3390000; 3,390,000 kcal = 1 ton of resources

  ask oikoi
  [
    let sea-distance min-one-of sea-patches [distance myself]
    set R-sea (marine-food-resources - (0.01 * distance sea-distance))
    ifelse R-sea + other-calorie-sources < 0.8
    [
      set R-agriculture (1 - (R-sea + other-calorie-sources))
    ]
    [
      set R-agriculture 0.2
    ]

    set oikoi_energy-requirements 7300000 * R-agriculture
    set oikoi_surplus 0
    ifelse ((oikoi_harvest + oikoi_stocks) >= oikoi_energy-requirements)
    [
      set oikoi_stocks oikoi_stocks + (oikoi_harvest - oikoi_energy-requirements)
    set oikoi_shortage 0
    ]
    [
      set oikoi_stocks 0
      set oikoi_shortage (oikoi_energy-requirements - (oikoi_harvest + oikoi_stocks))
    ]
    if (oikoi_stocks > storagelimit)
    [set oikoi_surplus (oikoi_stocks - storagelimit)
      set oikoi_stocks storagelimit
    ]
  ]
end


  ;;;;;; COMMERCE & EMIGRATION  ;;;;;;

to exchanges-among-neighbors?

  ask oikoi
  [
    set maxdebt max-debt * 3700000
    ifelse (oikoi_surplus >= debt)
    [
      set oikoi_surplus oikoi_surplus - debt set debt 0
    ]
    [
      set debt debt - oikoi_surplus set oikoi_surplus 0
    ]
  ]

  ask oikoi
  [
    if (oikoi_surplus > 0)
    [
      let debtor one-of oikoi in-radius settlement-perimeter with [oikoi_shortage > 0 and debt < maxdebt]

      if (debtor != nobody)
      [
        ask debtor [set loan (maxdebt - debt)]
      ]

       if (debtor != nobody and oikoi_surplus >= [loan] of debtor)
      [
        ask debtor
        [
          ifelse (loan >= oikoi_shortage)
          [
            set debt debt + oikoi_shortage
            set oikoi_shortage 0
          ]
          [
            set oikoi_shortage oikoi_shortage - loan
            set debt debt + loan
          ]
         ]
        set oikoi_surplus oikoi_surplus - loan
      ]

      if (debtor != nobody and oikoi_surplus < [loan] of debtor)
      [let s [oikoi_surplus] of self
           ask debtor
        [
          set loan s
          ifelse (loan >= oikoi_shortage)
          [
            set debt debt + oikoi_shortage
            set oikoi_shortage 0
          ]
          [
            set oikoi_shortage oikoi_shortage - loan
            set debt debt + loan
          ]
         ]
        set oikoi_surplus oikoi_surplus - loan
        ]
       ]
      ]
end

to simulate-external-exchanges

  set trade-flows 0
  ask oikoi
  [
    let adjusted-radius-commerce ( radius-commerce / real-size-km)
    if (oikoi_surplus > 0)
    [
      let my-neighbors [who] of oikoi in-radius settlement-perimeter
      let not-neighbors oikoi with [who != my-neighbors]
      let customer one-of not-neighbors in-radius adjusted-Radius-Commerce with [oikoi_surplus > 0]

      if (customer != nobody)
      [
      face customer
      set customer-distance [distance customer] of self
      ifelse (any? patches with [Safe? = false] in-cone customer-distance 15)
        [
          set safe-route? false
          let other-customer? one-of not-neighbors in-radius (adjusted-Radius-Commerce * 0.5) with [oikoi_surplus > 0]
          if (other-customer? != nobody)
          [
          face other-customer?
          set customer-distance [distance other-customer?] of self
          ifelse (any? patches with [Safe? = false] in-cone customer-distance 15)
            [set safe-route? false]
            [set safe-route? true]
          ]
         ]
        [set safe-route? true]

      if (safe-route? = true)
        [
        let trading-costs ((sum [cost-slope-p] of land-patches in-cone customer-distance 15) * real-size-km) + ((sum [cost-slope-N] of land-patches in-cone customer-distance 15)* real-size-km)
        set net-oikoi_surplus (oikoi_surplus -  trading-costs)
        if (net-oikoi_surplus > 0)
          [
            set trade-flows trade-flows + 1 let commerce-area customer-distance
          ]
         ]
        ]
       ]
      ]

end


to simulate-migrations

  let axcor 0
  let aycor 0
  let oxcor 0
  let oycor 0
  set n-migrations 0

  ask oikoi
  [
    let expected-yield-here [expected-yield] of patch-here
    ifelse (oikoi_harvest < oikoi_energy-requirements or [safe?] of patch-here = false)
    [set emigrate? true]
    [set emigrate? false set residence-continuity residence-continuity + 1]

    if (emigrate? = true)
    [
     set axcor [pxcor] of patch-here
     set aycor [pycor] of patch-here
     set target-patch min-one-of land-patches in-radius (50 / real-size-km) with [expected-yield > expected-yield-here and safe? = true]
          [distance myself]
      if target-patch != nobody
      [
        move-to target-patch
        set oxcor [pxcor] of patch-here
        set oycor [pycor] of patch-here
        ifelse ((oxcor != axcor) or (oycor != aycor))
        [
        set n-migrations n-migrations + 1
        set residence-continuity 1
        let sea-distance min-one-of sea-patches [distance myself]
        set R-sea (marine-food-resources - (0.01 * distance sea-distance))
        ifelse (R-sea + other-calorie-sources < 0.8)
          [
            set R-agriculture (1 - (R-sea + other-calorie-sources))
          ]
          [
            set R-agriculture 0.2
          ]
          set oikoi_energy-requirements 7300000 * R-agriculture
        ]
        [
          set residence-continuity residence-continuity + 1
        ]

        if (my-city != nobody)
        [
        let a [who] of self
        let b [who] of my-city
        if is-link? link a b [ask link a b [die]]
        set my-city min-one-of (settlements) with [destroyed? = false]
          [distance myself]
        ifelse ((distance my-city) < settlement-perimeter) [create-link-with my-city set no-city false]
          [set no-city true]
         ]
        ]
       ]

    ask oikoi with [no-city = true]
    [
    let n-households-without-city count oikoi in-radius settlement-perimeter with [no-city = true]
    if (n-households-without-city > 2)
      [
        ask one-of oikoi in-radius settlement-perimeter with [no-city = true]
        [
          let x [pxcor] of patch-here
          let y [pycor] of patch-here
          hatch-settlements 1
          [ setxy x y
            set shape "house"
            set size (5 / Cell-Size)
            set color red + 2
            set attacked? false
            set destroyed? false
            set safe? true
        ifelse (any? sea-patches in-radius coastal-area)
           [set new-coastal-cities new-coastal-cities + 1]
        [set new-inland-cities new-inland-cities + 1]
          ]

          set my-city one-of (settlements-on patch x y)
          set no-city false create-link-with my-city]
         ]
       ]
      ]
end


;;;;;; CITY & POPULATION DYNAMICS  ;;;;;;



to simulate-city-dynamics


  ask settlements
  [
    let n-households count oikoi in-radius settlement-perimeter
    set n-settlements count settlements
    set deffense-from-oikoi_surplus sum [oikoi_surplus] of oikoi in-radius settlement-perimeter
    set deffense-from-population-size count oikoi in-radius settlement-perimeter
    set deffense-from-orography [slope] of patch-here
    let max-deffense-from-oikoi_surplus max [deffense-from-oikoi_surplus] of settlements in-radius (50 / real-size-km)
    let max-deffense-from-population-size max [deffense-from-population-size] of settlements in-radius (50 / real-size-km)
    if max-deffense-from-population-size < 1 [set max-deffense-from-population-size 1]

    ifelse (deffense-from-oikoi_surplus > 0)
    [
      set defense-capability ((deffense-from-oikoi_surplus / max-deffense-from-oikoi_surplus)* (1 / 3)) + ((deffense-from-population-size / max-deffense-from-population-size) * (1 / 3)) + ((deffense-from-orography / 15) * (1 / 3))
    ]
    [
      set defense-capability (((deffense-from-population-size / max-deffense-from-population-size) * 0.5) + ((deffense-from-orography / 15) * 0.5))
    ]
    while [ n-households < 1 or destroyed? = true]
    [
      ifelse (any? sea-patches in-radius coastal-area)
      [
        set abandoned-coastal-cities abandoned-coastal-cities + 1 die
      ]
      [
        set abandoned-inland-cities abandoned-inland-cities + 1 die
      ]
     ]
    if (attacked? = true and destroyed? = false)
    [
      ifelse (random-float 1 < (1 - defense-capability))
      [
        set destroyed? true set shape "fire" set size 2 set color yellow ask oikoi in-radius settlement-perimeter [set oikoi_surplus 0 set oikoi_stocks 0]
      ]
      [
        set destroyed? false set shape "x" set size 2 set color red
      ]
     ]
    ifelse (destroyed? = true)
    [
      ask patches in-radius settlement-perimeter [set safe? false]
    ]
    [
      ask patches in-radius settlement-perimeter [set safe? true]
    ]
   ]
end


to simulate-population-dynamics


  ask oikoi
  [
    let adjusted-deathrate (( 1 + (oikoi_shortage / oikoi_energy-requirements)) * death-rate)
    if (any? seapeoples in-radius settlement-perimeter)
    [
      ifelse my-city != nobody
      [set adjusted-deathrate adjusted-deathrate * 2]
      [set adjusted-deathrate adjusted-deathrate * 3]
    ]
    if (random-float 1000 < adjusted-deathrate)
    [die]
   ]

  ask oikoi
  [
    let energetic-balance ( (oikoi_harvest + oikoi_stocks) - oikoi_energy-requirements)
    if (energetic-balance > 0)
    [
      let max-e max [energetic-balance] of oikoi in-radius settlement-perimeter
      let P count oikoi in-radius settlement-perimeter
      let K abs (sum [soil-capacity] of patches in-radius settlement-perimeter * 3390 / mean [oikoi_energy-requirements] of oikoi in-radius settlement-perimeter)
      let adjusted-birthrate ((energetic-balance / max-e) * birth-rate) * (1 - (P / K))
      if (random-float 1000 < adjusted-birthrate) [new-oikos]
    ]
   ]

end

to new-oikos

   hatch 1
  [
    rt random-float 360 fd random-float 1
      if ([pcolor] of patch-here = blue) [move-to one-of land-patches]
      set size (2 / cell-size)
      set color white
      set residence-continuity 1
      set debt 0
      set oikoi_stocks 0
      set oikoi_surplus 0
      set extension-cultivated 2
      set oikoi-per-patch count oikoi-here
      ifelse (any? settlements with [destroyed? = false] in-radius settlement-perimeter)
    [
      set my-city min-one-of (settlements) with [destroyed? = false]
      [distance myself]
      create-link-with my-city set no-city false
    ]
    [
      set no-city true
      let n-households-without-city count oikoi in-radius settlement-perimeter with [no-city = true]
      if (n-households-without-city > 2)
      [
        ask one-of oikoi in-radius settlement-perimeter with [no-city = true]
        [
          let x [pxcor] of patch-here let y [pycor] of patch-here
          hatch-settlements 1
          [
            setxy x y
            set shape "house"
            set size (5 / Cell-Size)
            set color red + 2
            set attacked? false
            set destroyed? false
            set Safe? true
            set settlement-elevation [elevation] of patch-here
        ifelse (any? sea-patches in-radius coastal-area)
            [set new-coastal-cities new-coastal-cities + 1]
            [set new-inland-cities new-inland-cities + 1]
           ]
          set my-city one-of (settlements-on patch x y)
          set no-city false create-link-with my-city
          ]
        ]
       ]
     ]

end


  ;;;;;; SEA PEOPLES  ;;;;;;



to raiding

  ask seapeoples
  [
    set city-target-patch nobody
    ifelse (any? settlements with [attacked? = false] in-radius settlement-perimeter)
    [
      set city-target-patch [who] of min-one-of settlements with [attacked? = false] [distance myself]
      ask settlements with [who = one-of [city-target-patch] of seapeoples] [set attacked? true]
    ]
    [
      right random 360
      forward 2
    ]
      if ([pcolor] of patch-here != blue)
    [
      move-to min-one-of sea-patches [distance myself]
    ]
   ]

   if (halo?)
  [
    ask halos [ move-to my-owner]
  ]

end



to make-halo ; for visual representation purposes

  hatch-halos 1
  [
    set size (15 / cell-size)
    set shape "Radius"
    set color lput 64 extract-rgb color
    __set-line-thickness 0.25
    set my-owner myself
    if my-owner = nobody [die]
  ]

end


 ;;;;;;  PLOTS  ;;;;;;


to do-plots

  set-current-plot "Population"
  set-current-plot-pen "Households"
  plot count oikoi

  set-current-plot "Settlements"
  set-current-plot-pen "coastal-settlements"
  plot count settlements with [any? sea-patches in-radius coastal-area]
  set-current-plot-pen "inland-settlements"
  plot count settlements with [count sea-patches in-radius coastal-area < 1]

  set-current-plot "Abandoned settlements"
  set-current-plot-pen "coastal-settlements"
  plot abandoned-coastal-cities
  set-current-plot-pen "inland-settlements"
  plot abandoned-inland-cities

  set-current-plot "Migrations"
  set-current-plot-pen "n-migrations"
  if (calendar < 1346) [plot n-migrations]

  set-current-plot "Extension cultivated"
  set-current-plot-pen "extension-cultivated"
  if (count oikoi > 0) [plot mean [extension-cultivated] of oikoi]

  set-current-plot "Rain Anomaly Index"
  set-current-plot-pen "rainfall"
  plot item cRAI RAI

  set-current-plot "Commerce"
  set-current-plot-pen "trade-flows"
  plot trade-flows
  set-current-plot-pen "trade-distance"
  if (count oikoi > 0) [plot (mean [customer-distance] of oikoi) * Real-Size-km]

  set-current-plot "Settlement elevation"
  set-current-plot-pen "elevation"
  if (count settlements > 1) [plot ( mean [elevation] of settlements)]

end
@#$#@#$#@
GRAPHICS-WINDOW
480
41
988
458
-1
-1
4.0
1
10
1
1
1
0
0
0
1
0
124
0
101
0
0
0
Calendar
30.0

BUTTON
267
22
338
60
NIL
setup\n
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
338
22
412
60
NIL
go\n
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
91
55
248
88
number-oikoi
number-oikoi
100
300
200.0
5
1
NIL
HORIZONTAL

SLIDER
64
346
251
379
rainfall-variability
rainfall-variability
0
0.2
0.05
0.05
1
NIL
HORIZONTAL

SLIDER
27
384
213
417
degradation-rate
degradation-rate
0
2
1.0
0.5
1
NIL
HORIZONTAL

SLIDER
68
421
249
454
recovery-rate
recovery-rate
0
1
0.1
0.1
1
NIL
HORIZONTAL

SLIDER
66
494
255
527
depletion-rate
depletion-rate
0
1
0.3
0.1
1
NIL
HORIZONTAL

SLIDER
27
457
212
490
farming-assets-factor
farming-assets-factor
0.5
2
1.0
0.5
1
NIL
HORIZONTAL

SLIDER
23
284
209
317
max-debt
max-debt
0
1.5
1.0
0.5
1
NIL
HORIZONTAL

CHOOSER
279
71
391
116
view
view
"Elevation" "soil-capacity" "RAI"
0

SLIDER
22
529
207
562
storage-limit
storage-limit
0.5
2
1.5
0.5
1
tons
HORIZONTAL

MONITOR
390
401
449
450
Years BC
Calendar
1
1
12

SWITCH
271
158
375
191
sea-people?
sea-people?
0
1
-1000

TEXTBOX
587
10
911
60
***BACO: Bronze Age Collapse***
20
63.0
1

SLIDER
68
154
252
187
marine-food-resources
marine-food-resources
0
0.8
0.2
0.1
1
NIL
HORIZONTAL

SLIDER
25
190
210
223
other-calorie-sources
other-calorie-sources
0
.8
0.1
.1
1
NIL
HORIZONTAL

PLOT
1081
460
1280
611
Population
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
"Households" 1.0 0 -7500403 true "" ""

PLOT
1000
178
1279
318
Settlements
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
"coastal-settlements" 1.0 0 -13345367 true "" ""
"inland-settlements" 1.0 0 -15040220 true "" ""

PLOT
480
461
680
611
Migrations
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
"n-migrations" 1.0 1 -16777216 true "" ""

PLOT
681
461
881
611
Extension cultivated
NIL
NIL
0.0
10.0
2.0
10.0
true
false
"" ""
PENS
"extension-cultivated" 10.0 2 -16777216 true "" ""

SLIDER
74
567
249
600
max-extension
max-extension
2
10
10.0
1
1
ha
HORIZONTAL

PLOT
880
461
1080
611
Rain Anomaly Index
NIL
NIL
0.0
10.0
0.0
4.0
true
false
"" ""
PENS
"rainfall" 1.0 2 -16777216 true "" ""

PLOT
1000
37
1279
179
Abandoned settlements
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
"coastal-settlements" 1.0 1 -16777216 true "" ""
"inland-settlements" 1.0 1 -8053223 true "" ""

SLIDER
306
223
439
256
number-sea-people
number-sea-people
10
30
20.0
1
1
NIL
HORIZONTAL

PLOT
1000
318
1279
458
Commerce
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
"trade-flows" 1.0 0 -16777216 true "" ""
"trade-distance" 1.0 0 -9276814 true "" ""

SLIDER
27
92
119
125
death-rate
death-rate
0
100
50.0
1
1
NIL
HORIZONTAL

SLIDER
120
92
212
125
birth-rate
birth-rate
0
100
65.0
1
1
NIL
HORIZONTAL

SLIDER
70
247
251
280
radius-commerce
radius-commerce
1
200
150.0
1
1
km
HORIZONTAL

TEXTBOX
89
25
228
46
----Population----
16
0.0
1

TEXTBOX
110
131
182
149
----Diet----
16
0.0
1

TEXTBOX
81
225
229
243
----Commerce----
16
0.0
1

TEXTBOX
71
321
221
346
----Rain&Farming----
16
0.0
1

INPUTBOX
294
275
357
335
cell-size
2.0
1
0
Number

MONITOR
372
288
455
333
NIL
real-size-km
1
1
11

SWITCH
296
191
444
224
activate-commerce?
activate-commerce?
0
1
-1000

PLOT
279
461
479
611
Settlement elevation
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
"elevation" 1.0 0 -16777216 true "" ""

SWITCH
313
126
438
159
update-view
update-view
0
1
-1000

INPUTBOX
285
337
372
397
beginning-raids
1220.0
1
0
Number

INPUTBOX
374
337
449
397
end-raids
1150.0
1
0
Number

BUTTON
412
27
467
60
NIL
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
374
158
464
191
halo?
halo?
0
1
-1000

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
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

boat
false
0
Polygon -1 true false 63 162 90 207 223 207 290 162
Rectangle -6459832 true false 150 32 157 162
Polygon -13345367 true false 150 34 131 49 145 47 147 48 149 49
Polygon -7500403 true true 158 33 230 157 182 150 169 151 157 156
Polygon -7500403 true true 149 55 88 143 103 139 111 136 117 139 126 145 130 147 139 147 146 146 149 55

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

cylinder
false
0
Circle -7500403 true true 0 0 300

dog
false
0
Polygon -7500403 true true 300 165 300 195 270 210 183 204 180 240 165 270 165 300 120 300 0 240 45 165 75 90 75 45 105 15 135 45 165 45 180 15 225 15 255 30 225 30 210 60 225 90 225 105
Polygon -16777216 true false 0 240 120 300 165 300 165 285 120 285 10 221
Line -16777216 false 210 60 180 45
Line -16777216 false 90 45 90 90
Line -16777216 false 90 90 105 105
Line -16777216 false 105 105 135 60
Line -16777216 false 90 45 135 60
Line -16777216 false 135 60 135 45
Line -16777216 false 181 203 151 203
Line -16777216 false 150 201 105 171
Circle -16777216 true false 171 88 34
Circle -16777216 false false 261 162 30

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

fire
false
0
Polygon -7500403 true true 151 286 134 282 103 282 59 248 40 210 32 157 37 108 68 146 71 109 83 72 111 27 127 55 148 11 167 41 180 112 195 57 217 91 226 126 227 203 256 156 256 201 238 263 213 278 183 281
Polygon -955883 true false 126 284 91 251 85 212 91 168 103 132 118 153 125 181 135 141 151 96 185 161 195 203 193 253 164 286
Polygon -2674135 true false 155 284 172 268 172 243 162 224 148 201 130 233 131 260 135 282

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

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

horse
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 165 165 135 165 90 165 75 150 72 211 49 209 48 181 37 149 25 120 25 89 60 75 120 90 165 90 198 76 255 30 270 45 293 103 285 121 255 90 242 118 210 150
Polygon -7500403 true true 73 210 90 240 60 240 45 195
Polygon -7500403 true true 25 114 15 210 15 210 30 165 30 165 39 123

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

house two story
false
0
Polygon -7500403 true true 2 180 227 180 152 150 32 150
Rectangle -7500403 true true 270 75 285 255
Rectangle -7500403 true true 75 135 270 255
Rectangle -16777216 true false 124 195 187 256
Rectangle -16777216 true false 210 195 255 240
Rectangle -16777216 true false 90 150 135 180
Rectangle -16777216 true false 210 150 255 180
Line -16777216 false 270 135 270 255
Rectangle -7500403 true true 15 180 75 255
Polygon -7500403 true true 60 135 285 135 240 90 105 90
Line -16777216 false 75 135 75 180
Rectangle -16777216 true false 30 195 93 240
Line -16777216 false 60 135 285 135
Line -16777216 false 255 105 285 135
Line -16777216 false 0 180 75 180
Line -7500403 true 60 195 60 240
Line -7500403 true 154 195 154 255

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

moose
false
0
Polygon -7500403 true true 196 228 198 297 180 297 178 244 166 213 136 213 106 213 79 227 73 259 50 257 49 229 38 197 26 168 26 137 46 120 101 122 147 102 181 111 217 121 256 136 294 151 286 169 256 169 241 198 211 188
Polygon -7500403 true true 74 258 87 299 63 297 49 256
Polygon -7500403 true true 25 135 15 186 10 200 23 217 25 188 35 141
Polygon -7500403 true true 270 150 253 100 231 94 213 100 208 135
Polygon -7500403 true true 225 120 204 66 207 29 185 56 178 27 171 59 150 45 165 90
Polygon -7500403 true true 225 120 249 61 241 31 265 56 272 27 280 59 300 45 285 90

orbit 3
true
0
Circle -7500403 true true 116 11 67
Circle -7500403 true true 26 176 67
Circle -7500403 true true 206 176 67
Circle -7500403 false true 45 45 210

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

radius
true
0
Circle -7500403 false true 15 15 270

sea-people
true
0
Line -7500403 true 0 120 30 165
Line -7500403 true 30 165 45 180
Line -7500403 true 45 180 60 195
Line -7500403 true 60 195 90 210
Line -7500403 true 90 210 135 210
Line -7500403 true 135 210 225 210
Line -7500403 true 225 210 255 195
Line -7500403 true 255 195 270 180
Line -7500403 true 270 180 285 165
Line -7500403 true 285 165 300 135
Line -7500403 true 0 120 45 150
Line -7500403 true 45 150 75 165
Line -7500403 true 75 165 135 180
Line -7500403 true 135 180 165 180
Line -7500403 true 255 165 300 135
Line -7500403 true 255 165 225 180
Line -7500403 true 165 180 225 180
Line -6459832 false 15 210 75 165
Line -6459832 false 45 240 105 180
Line -6459832 false 105 240 135 195
Line -6459832 false 180 195 150 240
Line -6459832 false 240 180 180 255

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

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

tile stones
false
0
Polygon -7500403 true true 0 240 45 195 75 180 90 165 90 135 45 120 0 135
Polygon -7500403 true true 300 240 285 210 270 180 270 150 300 135 300 225
Polygon -7500403 true true 225 300 240 270 270 255 285 255 300 285 300 300
Polygon -7500403 true true 0 285 30 300 0 300
Polygon -7500403 true true 225 0 210 15 210 30 255 60 285 45 300 30 300 0
Polygon -7500403 true true 0 30 30 0 0 0
Polygon -7500403 true true 15 30 75 0 180 0 195 30 225 60 210 90 135 60 45 60
Polygon -7500403 true true 0 105 30 105 75 120 105 105 90 75 45 75 0 60
Polygon -7500403 true true 300 60 240 75 255 105 285 120 300 105
Polygon -7500403 true true 120 75 120 105 105 135 105 165 165 150 240 150 255 135 240 105 210 105 180 90 150 75
Polygon -7500403 true true 75 300 135 285 195 300
Polygon -7500403 true true 30 285 75 285 120 270 150 270 150 210 90 195 60 210 15 255
Polygon -7500403 true true 180 285 240 255 255 225 255 195 240 165 195 165 150 165 135 195 165 210 165 255

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

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.3
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
