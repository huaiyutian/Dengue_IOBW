load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"

begin
; ==============================================================
; User defined parameters that specify region of globe and

  latS_SST   =  -20. ;Tropical Indian Ocean
  latN_SST   =  20.  
  lonL_SST   =  40.
  lonR_SST   =  110.
  

  latS_T     =  -60  ;Study Area Countries
  latN_T     =  60 
  lonL       = -180.
  lonR       = 180.
  
  yrStrt     = 1989
  yrLast     = 2018

  ;Please modify the absolute path
  f_SST      = addfile ("G:/Fig_2/04_Fig_2C_CCA/01_sst.nc", "r")
  f_T        = addfile ("G:/Fig_2/04_Fig_2C_CCA/02_2mt.nc", "r")
  
  ;Same time for both data
  TIME       = f_SST->time
  utc_date   = cd_calendar(TIME, 0)
  YYYY       = tointeger(utc_date(:,0))    ; Convert to integer for
  MM         = tointeger(utc_date(:,1))    ; use sprinti 

  ;==============================================================
  ;Autumn SST
  iYYYYMM_9     = ind(YYYY.ge.yrStrt .and. YYYY.le.yrLast .and. MM.eq.9)   
  iYYYYMM_10    = ind(YYYY.ge.yrStrt .and. YYYY.le.yrLast .and. MM.eq.10)   
  iYYYYMM_11    = ind(YYYY.ge.yrStrt .and. YYYY.le.yrLast .and. MM.eq.11)   
  ;Winter T
  iYYYYMM_12    = ind(YYYY.ge.yrStrt .and. YYYY.le.yrLast .and. MM.eq.12)   
  iYYYYMM_1     = ind(YYYY.ge.(yrStrt+1) .and. YYYY.le.(yrLast+1) .and. MM.eq.1)  
  iYYYYMM_2     = ind(YYYY.ge.(yrStrt+1) .and. YYYY.le.(yrLast+1) .and. MM.eq.2)  
  ;Spring SST
  iYYYYMM_3     = ind(YYYY.ge.(yrStrt+1) .and. YYYY.le.(yrLast+1) .and. MM.eq.3)  
  iYYYYMM_4     = ind(YYYY.ge.(yrStrt+1) .and. YYYY.le.(yrLast+1) .and. MM.eq.4)   
  iYYYYMM_5     = ind(YYYY.ge.(yrStrt+1) .and. YYYY.le.(yrLast+1) .and. MM.eq.5)   
  ;Summer T
  iYYYYMM_6     = ind(YYYY.ge.(yrStrt+1) .and. YYYY.le.(yrLast+1) .and. MM.eq.6)   
  iYYYYMM_7     = ind(YYYY.ge.(yrStrt+1) .and. YYYY.le.(yrLast+1) .and. MM.eq.7)   
  iYYYYMM_8     = ind(YYYY.ge.(yrStrt+1) .and. YYYY.le.(yrLast+1) .and. MM.eq.8)   
  
  ;Extraction of SST
  sst_ori_9     = f_SST->sst(iYYYYMM_9,:,:)  
  sst_ori_10    = f_SST->sst(iYYYYMM_10,:,:)  
  sst_ori_11    = f_SST->sst(iYYYYMM_11,:,:) 
  sst_ori_3     = f_SST->sst(iYYYYMM_3,:,:) 
  sst_ori_4     = f_SST->sst(iYYYYMM_4,:,:) 
  sst_ori_5     = f_SST->sst(iYYYYMM_5,:,:) 
  ;printVarSummary(sst_ori_3)

  ;Extraction of T
  t2m_ori_6    = f_T->t2m(iYYYYMM_6,:,:) 
  t2m_ori_7    = f_T->t2m(iYYYYMM_7,:,:) 
  t2m_ori_8    = f_T->t2m(iYYYYMM_8,:,:)  
  t2m_ori_12   = f_T->t2m(iYYYYMM_12,:,:) 
  t2m_ori_1    = f_T->t2m(iYYYYMM_1,:,:)  
  t2m_ori_2    = f_T->t2m(iYYYYMM_2,:,:)  

  ;Because of the scale factor, etc., the original data needs to be adjusted with byte2flt + converted to Celsius
  sst_9       = byte2flt(sst_ori_9) - 273.15 
  sst_10      = byte2flt(sst_ori_10) - 273.15 
  sst_11      = byte2flt(sst_ori_11) - 273.15 
  sst_3       = byte2flt(sst_ori_3) - 273.15 
  sst_4       = byte2flt(sst_ori_4) - 273.15 
  sst_5       = byte2flt(sst_ori_5) - 273.15 

  t2m_6       = byte2flt(t2m_ori_6) - 273.15 
  t2m_7       = byte2flt(t2m_ori_7) - 273.15 
  t2m_8       = byte2flt(t2m_ori_8) - 273.15 
  t2m_12      = byte2flt(t2m_ori_12) - 273.15 
  t2m_1       = byte2flt(t2m_ori_1) - 273.15 
  t2m_2       = byte2flt(t2m_ori_2) - 273.15 

  sst_autumn   = new((/30,721,1440/), float)
  sst_autumn = ( sst_9 +  sst_10 + sst_11 )/3.
  
  sst_spring   = new((/30,721,1440/), float)
  sst_spring = ( sst_3 +  sst_4 + sst_5 )/3.

  t2m_summer   = new((/30,721,1440/), float)
  t2m_summer = ( t2m_6 +  t2m_7 + t2m_8 )/3.
  
  t2m_winter   = new((/30,721,1440/), float)
  t2m_winter = ( t2m_12 +  t2m_1 + t2m_2 )/3.


  sst_autumn!0               ="time"
  sst_autumn!1               ="latitude"
  sst_autumn!2               ="longitude"
  sst_autumn&time            =sst_ori_9&time
  sst_autumn&latitude        =sst_ori_9&latitude
  sst_autumn&longitude       =sst_ori_9&longitude
  sst_autumn@_FillValue      =-32767
  sst_autumn@units           = "dgreed"
  ;printVarSummary(sst_autumn)

  sst_spring!0               ="time"
  sst_spring!1               ="latitude"
  sst_spring!2               ="longitude"
  sst_spring&time            =sst_ori_3&time
  sst_spring&latitude        =sst_ori_3&latitude
  sst_spring&longitude       =sst_ori_3&longitude
  sst_spring@_FillValue      =-32767
  sst_spring@units           = "dgreed"
  ;printVarSummary(sst_spring)

  t2m_summer!0               ="time"
  t2m_summer!1               ="latitude"
  t2m_summer!2               ="longitude"
  t2m_summer&time            =t2m_ori_7&time
  t2m_summer&latitude        =t2m_ori_7&latitude
  t2m_summer&longitude       =t2m_ori_7&longitude
  t2m_summer@_FillValue      =-32767
  t2m_summer@units           = "dgreed"
  ;printVarSummary(t2m_summer)

  t2m_winter!0               ="time"
  t2m_winter!1               ="latitude"
  t2m_winter!2               ="longitude"
  t2m_winter&time            =t2m_ori_12&time
  t2m_winter&latitude        =t2m_ori_12&latitude
  t2m_winter&longitude       =t2m_ori_12&longitude
  t2m_winter@_FillValue      =-32767
  t2m_winter@units           = "dgreed"
  ;printVarSummary(t2m_winter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ; Read U and calculate the correlation coefficient
  u_autumn                     = asciiread ("G:/Fig_2/04_Fig_2C_CCA/03_u-autumn.txt",30, "float")  
  ;print(u_autumn)
  
  ;New coordinate system
  ; !!! Don't new(...) again
  u_autumn!0                   = "time"
  u_autumn&time                = sst_autumn&time
  ;printVarSummary( u_autumn ) 
  ;print(u_autumn)
  r_u_autumn_sst               = escorc_n(u_autumn,sst_autumn,0,0)
  ;New coordinate system
  r_u_autumn_sst!0             = "latitude"
  r_u_autumn_sst!1             = "longitude"
  r_u_autumn_sst&latitude      = sst_autumn&latitude
  r_u_autumn_sst&longitude     = sst_autumn&longitude
  r_u_autumn_sst_S           =  r_u_autumn_sst({-20:20},{40:110})

  printVarSummary( r_u_autumn_sst_S )
  print( max(r_u_autumn_sst_S) )
  print( min(r_u_autumn_sst_S) )

;------------------------------------------------------------------------------------------
  ; Read U and calculate the correlation coefficient
  u_spring                   = asciiread ("G:/Fig_2/04_Fig_2C_CCA/04_u-summer.txt",30, "float")  
  ;print(u_spring)
  
  u_spring!0                 = "time"
  u_spring&time              = sst_spring&time
  ;printVarSummary( u_spring ) 
  ;print(u_spring)

  r_u_spring_sst  = escorc_n(u_spring,sst_spring,0,0)
  r_u_spring_sst!0           = "latitude"
  r_u_spring_sst!1           = "longitude"
  r_u_spring_sst&latitude    = sst_spring&latitude
  r_u_spring_sst&longitude   = sst_spring&longitude
  r_u_spring_sst_N           =  r_u_spring_sst({-20:20},{40:110})

  printVarSummary( r_u_spring_sst_N )
  print( max(r_u_spring_sst_N) )
  print( min(r_u_spring_sst_N) )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ; Read V and calculate the correlation coefficient
  v_winter                   = asciiread ("G:/Fig_2/04_Fig_2C_CCA/05_v-winter.txt",30, "float")  
  ;print(v_winter)
  
  v_winter!0                 = "time"
  v_winter&time              = t2m_winter&time
  ;printVarSummary( v_winter ) 
  ;print(v_winter)

  r_v_t2m_winter             = escorc_n(v_winter,t2m_winter,0,0)
  r_v_t2m_winter!0           = "latitude"
  r_v_t2m_winter!1           = "longitude"
  r_v_t2m_winter&latitude    = t2m_winter&latitude
  r_v_t2m_winter&longitude   = t2m_winter&longitude
  ;printVarSummary( r_v_t2m_winter )

  r_v_t2m_winter_S           =  r_v_t2m_winter({0:latS_T},:)
  printVarSummary( r_v_t2m_winter_S )

;------------------------------------------------------------------------------------------
  ; Read V and calculate the correlation coefficient
  v_summer                   = asciiread ("G:/Fig_2/04_Fig_2C_CCA/06_v-summer.txt",30, "float")  
  ;print(v_summer)
  
  v_summer!0                 = "time"
  ;v_summer&time              = t2m_summer&time
  ;printVarSummary( v_summer ) 
  ;print(v_summer)

  r_v_t2m_summer             = escorc_n(v_summer,t2m_summer,0,0)
  r_v_t2m_summer!0           = "latitude"
  r_v_t2m_summer!1           = "longitude"
  r_v_t2m_summer&latitude    = t2m_summer&latitude
  r_v_t2m_summer&longitude   = t2m_summer&longitude
  ;printVarSummary( r_v_t2m_summer )

  r_v_t2m_summer_N         =  r_v_t2m_summer({latN_T:0},:)
  printVarSummary( r_v_t2m_summer_N )
  
  print( max(r_v_t2m_summer_N) )
  print( min(r_v_t2m_summer_N) )
  print( max(r_v_t2m_winter_S) )
  print( min(r_v_t2m_winter_S) )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;Drawing
  
;************************************************
; Indicate the geographic area you want to fill in
; then indicate the geographic area to be blocked
;************************************************
  ;fill_specs = (/"land"/)
  mask_specs = (/"Antigua and Barbuda","Argentina","Bangladesh","Barbados","Belize","Bhutan","Bolivia","Brazil","Cambodia","Chile",\
  "Colombia","Costa Rica","Cuba","Dominica","Dominican Republic","Ecuador","El Salvador","Grenada","Guatemala","Guyana",\
  "Honduras","India","Indonesia","Jamaica","Laos","Malaysia","Mexico","Myanmar","Nepal",\
  "Nicaragua","Panama","Paraguay","Peru","Philippines","Saint Kitts and Nevis","Saint Lucia","Saint Vincent and the Grenadines","Singapore","Sri Lanka (Ceylon)",\
  "Suriname","Thailand","Trinidad and Tobago","United States","Uruguay","Venezuela","Vietnam"/)
  
  
;************************************************
; Open the workstation and define the color map
;************************************************
  wks_type           = "pdf"        ; or "pdf"
  ;wks_type@wkWidth  = 2500
  ;wks_type@wkHeight = 2500 
  wks                = gsn_open_wks(wks_type,"G:/Fig_2/04_Fig_2C_CCA/09_Fig_2C")   ; send graphics to PNG file

 ; plot = new(4,graphic)                ; create graphic array
  color_map          = (/(/1.00,1.00,1.00/),(/0.00,0.00,0.00/),(/0.90,0.90,0.90/)/)
  gsn_define_colormap(wks,color_map)


;==============================================================================================
; Draw T

;************************************************
; Set up some resources.
;************************************************
res_T_N                      = True 
res_T_N@gsnMaximize          = True
res_T_N@gsnAddCyclic         = False                ; gsnAddCyclic should be set to False as long as the data does not cover the entire planet

res_T_N@gsnDraw              = False                   ; do not draw the plot
res_T_N@gsnFrame             = False                   ; do not advance the frame

;************************************************
; Set up some contour resources
;************************************************
res_T_N@cnFillOn             = True          ; turn on color
res_T_N@cnFillPalette        = "BlueWhiteOrangeRed"     ; set color map
res_T_N@cnLinesOn            = False        ; True is default
res_T_N@cnLineLabelsOn       = False        ; True is default
;res_T_N@lbLabelBarOn         = False        ; turn off individual lb's
res_T_N@cnLevelSelectionMode = "ExplicitLevels"      ; manually set cn levels
res_T_N@cnLevels             = (/-0.85,-0.75,-0.65,-0.55,-0.45,-0.35,0.35,0.45,0.55,0.65,0.75,0.85/)
res_T_N@cnFillDrawOrder = "Predraw"

;************************************************
; Set some map latitude/longitude grid resources
;************************************************
res_T_N@mpLimitMode          = "LatLon"
res_T_N@mpMinLatF            =  latS_T        ; zoom in on map
res_T_N@mpMaxLatF            =  latN_T
res_T_N@mpMinLonF            =   0
res_T_N@mpMaxLonF            = 360
res_T_N@mpCenterLonF         = 180     ; This is necessary to get the correct map

res_T_N@mpProjection         = "Robinson"       ; choose projection
res_T_N@mpGridAndLimbOn      = True              ; Plotting latitude and longitude lines
res_T_N@mpPerimOn            = False             ; turn off box around plot
res_T_N@mpGridLatSpacingF    = 30.              ; spacing for lat lines
res_T_N@mpGridLonSpacingF    = 60.              ; spacing for lon lines
res_T_N@mpGridLineDashPattern= 2
res_T_N@mpGridLineColor      = 1
res_T_N@mpGridLineThicknessF = 0.5
res_T_N@mpGridMaskMode       = "MaskLand" ; Do not draw the grid on land
res_T_N@mpLimbLineThicknessF = 1   ;limb Edge line

;************************************************
; Open the mask and set the resource to indicate the area to be masked
;************************************************
  res_T_N@mpAreaMaskingOn         = True         ;The specified area remains unfilled
  res_T_N@mpMaskAreaSpecifiers    = mask_specs
  
;************************************************
; Draw the boundaries of the filled area
;************************************************
  res_T_N@mpDataBaseVersion       = "Ncarg4_1"                ;Medium resolution
  res_T_N@mpDataSetName           = "Earth..3"                ;Version 4 map
  res_T_N@mpOutlineOn             = True 
  res_T_N@mpLandFillColor           = 30          ; for land, ocean, and inland h20
  res_T_N@mpInlandWaterFillColor    = 0 
  res_T_N@mpOceanFillColor          = 0            ; array index in color map

;===============================================================================================
; Draw STT

;************************************************
; Set up some resources
;************************************************
res_T_S                      = True 
res_T_S@gsnMaximize          = True
res_T_S@gsnAddCyclic         = False                ;  As long as the data does not cover the entire globe, gsnAddCyclic should be set to False
res_T_S@gsnDraw              = False                   ; do not draw the plot
res_T_S@gsnFrame             = False                   ; do not advance the frame

;************************************************
; Set up some profile resources
;************************************************
res_T_S@cnFillOn             = True          ; turn on color
res_T_S@cnFillPalette        = "BlueWhiteOrangeRed"     ; set color map
res_T_S@cnLinesOn            = False        ; True is default
res_T_S@cnLineLabelsOn       = False        ; True is default
res_T_S@cnLevelSelectionMode = "ExplicitLevels"      ; manually set cn levels
res_T_S@cnLevels             = (/-0.85,-0.75,-0.65,-0.55,-0.45,-0.35,0.35,0.45,0.55,0.65,0.75,0.85/)

res_T_S@cnFillDrawOrder = "Predraw"

;===================================================================================

;************************************************
; Set up some tab bar resources.
; Note that this tab bar has two more boxes than our outline level.
; These extra boxes are used to display land and ocean colors.
;************************************************
res_T_N@lbLabelBarOn         = False        ; turn off individual lb's    
res_T_N@lbLabelAlignment = "BoxCenters"


  plot_T_N    = gsn_csm_contour_map(wks,r_v_t2m_summer_N,res_T_N)   ; create the temperature plot
  plot_T_S    = gsn_csm_contour(wks,r_v_t2m_winter_S,res_T_S)   ; create the temperature plot
  
  overlay(plot_T_N,plot_T_S)                       ; overlay the U-wind plot on the temperature plot
  draw(plot_T_N)                                  ; draw the temperature plot (with the U-wind plot overlaid)
  frame(wks)                                  ; advance the frame
end