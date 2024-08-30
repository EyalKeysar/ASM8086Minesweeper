;multi-segment executable file template.    
;Made by Eyal Keysar
;MINESWEEPER
 
 
 ;EQU'S SOME_EQU
 ;labels some_label
 ;variables SomeVar
 ;proc's someProcf
data segment
    ; add your data here! 
    ;variables    
    pkey db 08, "press any key...$"  
    ; 2d arrays to define image
 
	
   Flag2DArr  db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
              db 0, 0, 0, 4, 4, 4, 0, 0, 0, 0     
              db 0, 0, 4, 4, 4, 4, 0, 0, 0, 0
              db 4, 4, 4, 4, 4, 4, 0, 0, 0, 0
              db 0, 0, 4, 4, 4, 4, 0, 0, 0, 0
              db 0, 0, 0, 4, 4, 4, 0, 0, 0, 0
              db 0, 0, 0, 0, 4, 4, 0, 0, 0, 0
              db 0, 0, 0, 0, 4, 4, 0, 0, 0, 0
              db 0, 0, 0, 0, 4, 4, 0, 0, 0, 0
              db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0             
                     
    NumberOne2d db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0  
                db 0, 0, 0, 0, 9, 9, 0, 0, 0, 0   
                db 0, 0, 0, 0, 9, 9, 0, 0, 0, 0  
                db 0, 0, 0, 0, 9, 9, 0, 0, 0, 0  
                db 0, 0, 0, 0, 9, 9, 0, 0, 0, 0  
                db 0, 0, 0, 0, 9, 9, 0, 0, 0, 0  
                db 0, 0, 0, 0, 9, 9, 0, 0, 0, 0  
                db 0, 0, 0, 0, 9, 9, 0, 0, 0, 0  
                db 0, 0, 0, 0, 9, 9, 0, 0, 0, 0  
                db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0     
                      
   NumberTwo2d  db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0  
                db 0, 0, 2, 2, 2, 2, 0, 0, 0, 0  
                db 0, 2, 0, 0, 0, 0, 2, 0, 0, 0  
                db 0, 0, 0, 0, 0, 0, 2, 0, 0, 0  
                db 0, 0, 0, 0, 2, 2, 0, 0, 0, 0  
                db 0, 0, 2, 2, 0, 0, 0, 0, 0, 0  
                db 0, 2, 0, 0, 0, 0, 0, 0, 0, 0  
                db 0, 2, 0, 0, 0, 0, 0, 0, 0, 0  
                db 0, 0, 2, 2, 2, 2, 2, 2, 0, 0  
                db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0  
                                                                   
    NumberThree2d db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0   
                  db 0, 4, 4, 4, 4, 4, 4, 4, 0, 0    
                  db 0, 0, 0, 0, 0, 0, 0, 0, 4, 0
                  db 0, 0, 0, 0, 0, 0, 0, 0, 4, 0  
                  db 0, 4, 4, 4, 4, 4, 4, 4, 4, 0 
                  db 0, 0, 0, 0, 0, 0, 0, 0, 4, 0
                  db 0, 0, 0, 0, 0, 0, 0, 0, 4, 0  
                  db 0, 4, 4, 4, 4, 4, 4, 4, 0, 0   
                  db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0  
                  db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0  
                
    NumberFour2d db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0  
                 db 0, 6, 0, 0, 0, 0, 0, 0, 6, 0     
                 db 0, 6, 0, 0, 0, 0, 0, 0, 6, 0
                 db 0, 6, 0, 0, 0, 0, 0, 0, 6, 0
                 db 0, 6, 0, 0, 0, 0, 0, 0, 6, 0
                 db 0, 6, 6, 6, 6, 6, 6, 6, 6, 0
                 db 0, 0, 0, 0, 0, 0, 0, 0, 6, 0
                 db 0, 0, 0, 0, 0, 0, 0, 0, 6, 0
                 db 0, 0, 0, 0, 0, 0, 0, 0, 6, 0
                 db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0     
                             
    NumberFive2d db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                 db 0, 1, 1, 1, 1, 1, 1, 1, 1, 0     
                 db 0, 1, 0, 0, 0, 0, 0, 0, 0, 0
                 db 0, 1, 0, 0, 0, 0, 0, 0, 0, 0
                 db 0, 0, 1, 1, 1, 1, 1, 1, 0, 0
                 db 0, 0, 0, 0, 0, 0, 0, 0, 1, 0
                 db 0, 0, 0, 0, 0, 0, 0, 0, 1, 0
                 db 0, 0, 0, 0, 0, 0, 0, 0, 1, 0
                 db 0, 1, 1, 1, 1, 1, 1, 1, 0, 0 
                 db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0    
                 
    NumberSix2d db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                db 0, 0, 6, 6, 6, 6, 6, 6, 0, 0     
                db 0, 6, 0, 0, 0, 0, 0, 0, 0, 0
                db 0, 6, 0, 0, 0, 0, 0, 0, 0, 0
                db 0, 6, 6, 6, 6, 6, 6, 6, 0, 0
                db 0, 6, 0, 0, 0, 0, 0, 0, 6, 0
                db 0, 6, 0, 0, 0, 0, 0, 0, 6, 0
                db 0, 6, 0, 0, 0, 0, 0, 0, 6, 0
                db 0, 0, 6, 6, 6, 6, 6, 6, 0, 0  
                db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0   

    NumberSeven2d db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                  db 0, 0, 2, 2, 2, 2, 2, 0, 0, 0     
                  db 0, 0, 0, 0, 0, 0, 0, 2, 0, 0
                  db 0, 0, 0, 0, 0, 0, 0, 2, 0, 0
                  db 0, 0, 0, 0, 0, 0, 0, 2, 0, 0
                  db 0, 0, 0, 0, 0, 2, 2, 2, 2, 0
                  db 0, 0, 0, 0, 0, 0, 0, 2, 0, 0
                  db 0, 0, 0, 0, 0, 0, 0, 2, 0, 0
                  db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0   
                  db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0   

    NumberEight2d db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                  db 0, 0, 9, 9, 9, 9, 9, 9, 0, 0     
                  db 0, 9, 0, 0, 0, 0, 0, 0, 9, 0
                  db 0, 9, 0, 0, 0, 0, 0, 0, 9, 0
                  db 0, 0, 9, 9, 9, 9, 9, 9, 0, 0
                  db 0, 9, 0, 0, 0, 0, 0, 0, 9, 0
                  db 0, 9, 0, 0, 0, 0, 0, 0, 9, 0
                  db 0, 0, 9, 9, 9, 9, 9, 9, 0, 0  
                  db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0  
                  db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0     
				  

    ; Grid position        
    TopLeftPixelOfGridPX dw 105  ; Top left index x of the grid
    TopLeftPixelOfGridPY dw 80  ; Top left index y of the grid
    
	; Grid size
    CubeOpenCloseGrid db 100 dup(0)  ; Grid that represent if cubes are open or closed  
    NumGrid db 100 dup(0)  ; Grid that contains numbers -1 to 8 that each one represent the number of mines near this cube ( -1 is mine)
    GridLength dw 100  ; Length of the grid
    GridLineLength dw 10  ; Length of line in the grid
    
	;cube size
    CubeLengthByPixels dw 10  ; Length of cube side in pixels
	
	; Variables that returned from getRowAndColByNumInGrid
    RetRowByNumInGrid dw ?  ; Row number of given grid index   
    RetColByNumInGrid dw ?  ; Col number of given grid index   
    
	; Variables that returned from getNumInGridByRowAndCol
    RetNumByRowAndColInGrid dw ?  ; Grid number of given row and col
    
	; Variables that returned from getGridCubeByPXPY
    RetGXInGetGridCubeByPXPY dw ?  ; Grid col number by col and row of pixel
    RetGYInGetGridCubeByPXPY dw ?  ; Grid row number by col and row of pixel
     
	; Variables for generate mines
    NumOfMines dw 15  ; Number of mines to generate
    FirstPrimeForRandom dw 53003  ; Some random prime number to use to get 
    SecondPrimeForRandom dw 51869  ; Some random prime number
	
    NumOfFlags dw 0 ; Number of current placed flags        
    
	PlayerWon dw 0
    PlayerDied dw 0
    
    StartScreenString db " !- To start press mouse -! $"   
    GameScreenString db " Left button => open cube | Right button => place flag$"   
    DiedScreenString db " !! BOOM !! YOU DIED $"
	NumOfFlagsString db "Number of placed flags = $"
	WonScreenString db "YOU WON!!$"
	StringToRestartGame db "Left button => restart | Right button => exit $"
	
    
ends                                                       


stack segment
    dw  128  dup(0)                                       
ends

code segment      
	;EQU's 
	
	CUBE_IS_OPEN   EQU 1
	CUBE_IS_CLOSED EQU 0   
	CUBE_IS_FLAG EQU 2
	IS_MINE EQU -1    
	CUBE_IS_EMPTY EQU 0
	OUT_OF_BOUNDS EQU -1
	EMPTY_COLOR EQU 0   
	
	PLAYER_DIED EQU 1
	PLAYER_ALIVE EQU 0  
	PLAYER_NOT_WON EQU 0
	PLAYER_WON EQU 1
	
	; Colors
	GRAY_COLOR EQU 07H
	LIGHT_COLOR EQU 02h
	DARK_COLOR EQU 0ch       
	
	SECOND_COLOR_SCREEN_START equ 08h
    FIRST_COLOR_SCREEN_START equ 03h      
    
	FIRST_COLOR_SCREEN_GAME equ 0bh
	SECOND_COLOR_SCREEN_GAME equ 01h 
	 
	FIRST_COLOR_SCREEN_DIED equ 04h
	SECOND_COLOR_SCREEN_DIED equ 0Ch
	
	FIRST_COLOR_SCREEN_WON equ 01h
	SECOND_COLOR_SCREEN_WON equ 02h
    
	SCREEN_LINE_HIGH EQU 80
	SCREEN_LINE_LOW EQU 50  
	PRINT_SCREEN_TEXT_LINE EQU 7
	
	
	
;proc's    

; Procs that deals with index and convert from 2d array to 1d
; Procs that draws
; Scanners & Recursive
; Mathematical    
; Screen & Numbers printing
; Others
       

; Procs that deals with index and convert from 2d array to 1d =========================================================================================Procs that deals with index and convert from 2d array to 1d

; This proc gets screen x and y index and convert it to grid index x and y
PX_FOR_GET_GRID_CUBE_BY_PX_PY equ bp+6  ; Screen x index
PY_FOR_GET_GRID_CUBE_BY_PX_PY equ bp+4  ; Screen y index
proc getGridCubeByPXPY  
push bp
mov bp, sp
    push ax
    push dx
    
    mov ax, [PX_FOR_GET_GRID_CUBE_BY_PX_PY]  ; Divid x by 2 remove top left x index and divid by cube length
    shr ax, 1 
    sub ax, TopLeftPixelOfGridPX
    xor dx, dx             
    div CubeLengthByPixels 
    mov RetGXInGetGridCubeByPXPY, ax
    
    mov ax, [PY_FOR_GET_GRID_CUBE_BY_PX_PY]   ; Remove top left y index and divid by cube length
    sub ax, TopLeftPixelOfGridPY
    xor dx, dx          
    div CubeLengthByPixels 
    mov RetGYInGetGridCubeByPXPY, ax  
    
    pop dx
    pop ax    
pop bp
ret 4
endp getGridCubeByPXPY 


; This proc take col and row and convert the to grid number, if col or row or both out of the grid index it will return -1                                   
COL_FOR_GET_NUM_IN_GRID_BY_ROW_AND_COL EQU bp+6  ; Col to convert to grid number
ROW_FOR_GET_NUM_IN_GRID_BY_ROW_AND_COL EQU bp+4  ; Row to convert to grid number
proc getNumInGridByRowAndCol
    push bp
    mov bp, sp
    pusha 
	
	mov RetNumByRowAndColInGrid, OUT_OF_BOUNDS  
	   
	mov ax, [ROW_FOR_GET_NUM_IN_GRID_BY_ROW_AND_COL]  ; Cheack if row is out of bounds 
	mov bx, GridLineLength
	cmp ax, bx
	jge end_getNumInGridByRowAndCol
	cmp ax, 0
	jl end_getNumInGridByRowAndCol  
	
	mov ax, [COL_FOR_GET_NUM_IN_GRID_BY_ROW_AND_COL]  ; Cheack if out of bounds 
	mov bx, GridLineLength
	cmp ax, bx
	jge end_getNumInGridByRowAndCol
	cmp ax, 0
	jl end_getNumInGridByRowAndCol  
    
    mov ax, GridLineLength	; Line length mul by col number and add row number      
    xor dx, dx
    mul [ROW_FOR_GET_NUM_IN_GRID_BY_ROW_AND_COL] 
    add ax, [COL_FOR_GET_NUM_IN_GRID_BY_ROW_AND_COL]
    mov RetNumByRowAndColInGrid, ax
end_getNumInGridByRowAndCol:
    popa
    pop bp
    ret 4
endp getNumInGridByRowAndCol

; This proc gets a grid index number and return the index col and row indexes
NUM_FOR_GET_ROW_AND_COL_IN_GRID equ bp+4  ; Num to convert to col and row index
proc getRowAndColByNumInGrid
    push bp
    mov bp, sp
    pusha                   
    mov ax, [NUM_FOR_GET_ROW_AND_COL_IN_GRID]  ; Dividing the given num takes division   
    xor dx, dx     
    div GridLineLength     
    mov retRowByNumInGrid, ax 
	
    mov ax, [NUM_FOR_GET_ROW_AND_COL_IN_GRID] ; Dividing the given num  takes modulo
    xor dx, dx  
    div GridLineLength 
    mov retColByNumInGrid, dx
    
    popa
    pop bp
    ret 2;num of parameters*2
endp getRowAndColByNumInGrid 


; Procs that draws  ========================================================================================================================================================= Procs that draws

; This proc draw pixel at given pos  and with give color                            
VAR_X_FOR_DRAW_PIXEL equ bp+8  ; X screen index to draw pixel
VAR_Y_FOR_DRAW_PIXEL equ bp+6  ; Y screen index to draw pixel
COLOR_FOR_DRAW_PIXEL equ bp+4  ; Color to draw pixel
proc drawPixelAtPos
    push bp
    mov bp, sp
    pusha    
    mov ah, 0ch  ; Int that draw pixel        
    mov al, [COLOR_FOR_DRAW_PIXEL]
    mov bh, 0 
    mov cx, [VAR_X_FOR_DRAW_PIXEL]
    mov dx, [VAR_Y_FOR_DRAW_PIXEL]
    int 10h   
    popa
    pop bp
    ret 6
endp drawPixelAtPos

; This proc draws cube in grid at X and Y index
DRAW_CUBE_AT_POS_GX EQU bp+8 ; X grid index for draw cube
DRAW_CUBE_AT_POS_GY EQU bp+6 ; Y grid index for draw cube
DRAW_CUBE_AT_POS_COLOR EQU bp+4  ; Cube color
proc drawCubeAtPos
    push bp
    mov bp, sp   
    pusha   
    mov ax, 2  ; Hide cursor so it wont disturb the printing
    int 33h  
    mov si, 0 ; SI is the outter loop index
  start_outter_loop_in_draw_cube_at_pos:  
    mov ax, [DRAW_CUBE_AT_POS_GX]  ; Get correct x screen index to draw cur pixel
    mul CubeLengthByPixels
    add ax, TopLeftPixelOfGridPX
    add ax, si
      
    mov di, 0  ; DI is the inner loop index
  start_inner_loop_in_draw_cube_at_pos:    
    push ax  ; Get correct y screen index to draw cur pixel
    mov ax, [DRAW_CUBE_AT_POS_GY]
    mul CubeLengthByPixels
    add ax, di
    add ax, TopLeftPixelOfGridPY
    mov bx, ax
    pop ax      
    
    push ax  ; Draw pixel at ax, bx
    push bx
    push [DRAW_CUBE_AT_POS_COLOR]
    call drawPixelAtPos
   
    inc di
    cmp di, CubeLengthByPixels
    je end_inner_loop_in_draw_cube_at_pos  
    jmp start_inner_loop_in_draw_cube_at_pos
  end_inner_loop_in_draw_cube_at_pos:   
    inc si
    cmp si, CubeLengthByPixels
    je end_outter_loop_in_draw_cube_at_pos    
    jmp start_outter_loop_in_draw_cube_at_pos
  end_outter_loop_in_draw_cube_at_pos:
    mov ax, 1h  ; Show mouse
    int 33h  
    popa
    pop bp
    ret 6
endp drawCubeAtPos

; This proc prints costume 2d array of colors in given cube    
ARRAY_FOR_ARRAY_TO_PIXELS equ bp+8  ; Offset of wanted array 
GX_FOR_ARRAY_TO_PIXELS equ bp+6  ; Grid col index to print
GY_FOR_ARRAY_TO_PIXELS equ bp+4  ; Grid row index to print
proc arrayToPixels
    push bp
    mov bp, sp
    pusha  
    
    mov si, [ARRAY_FOR_ARRAY_TO_PIXELS] ; Si is the start index of the given array
    
    mov ax, 2 ; Hide cursor so it wont disturb the printing on the screen
    int 33h  
	
    mov cx, 0  ; CX is now the outter loop index
  start_outter_loop_arr_to_pixels:
    ; Calculate the screen X index of the cube
    
	mov ax, [GY_FOR_ARRAY_TO_PIXELS]   
    xor dx, dx
    mul CubeLengthByPixels
    add ax, cx
    add ax, TopLeftPixelOfGridPY
      
    mov di, 0 ; DI is now the index of inner loop
  start_inner_loop_arr_to_pixels:
    ; Calculate the screen Y index of the cube
    push ax
    mov ax, [GX_FOR_ARRAY_TO_PIXELS]
    xor dx, dx  ; We are multipling ax:dx
    mul CubeLengthByPixels
    add ax, di
    add ax, TopLeftPixelOfGridPX
    mov bx, ax
    pop ax 
   
    cmp [si], EMPTY_COLOR  ; If cur color is empty, dont print a color
    je end_draw_cur_pixel_in_arr_to_pixels    
   
    push bx  ; Prints cur pixel
    push ax
    push [si]
    call drawPixelAtPos  
   
  end_draw_cur_pixel_in_arr_to_pixels: 
   
    inc di  ; Next inner loop index
    inc si  ; Move to next color
    cmp di, CubeLengthByPixels
    je end_inner_loop_in_arr_to_pixels  
    jmp start_inner_loop_arr_to_pixels
  end_inner_loop_in_arr_to_pixels: 
      
    inc cx  ; Next outter loop index
    cmp cx, CubeLengthByPixels
    je end_outter_loop_in_arr_to_pixels    
    jmp start_outter_loop_arr_to_pixels
  end_outter_loop_in_arr_to_pixels:
    ;Active show mouse
    mov ax, 1h
    int 33h 
    
    popa    
    pop bp
    ret 6
endp arrayToPixels 

; This proc draw the map of the game, by both grids
proc drawMap
    pusha    
    mov cx, GridLineLength
    mov si, 0  ; SI is used for choosing between the two colors  when the cubes are close
  start_outter_loop_in_draw_map:    
    push cx
	
    mov ax, GridLineLength  ; Check if grid line is divideable by 2 so we can inc or not si value so its fits like a chess board
    mov bl, 02h
    div bl        
    cmp ah, 0                 
    jne length_divideable_by_2_draw_map  
    cmp si, 0  ; If line length is not divideable we need to change si
    je si_zero_to_one_draw_map   
	
    mov si, 0                      ; If si=1 change to 0
    jmp length_divideable_by_2_draw_map
	
  si_zero_to_one_draw_map:   
    mov si, 1                      ; If si=0 change to 1     

  length_divideable_by_2_draw_map:  ; End of changing si
    mov dx, cx ; Save current cx (index of outter loop)
    mov cx, GridLineLength  ; Now cx is the index for the inner loop
  start_inner_loop_in_draw_map: 
    push cx  ; loop indexes so we push and we will pop at end of inner loop
    push si  
    push di 
	
  start_draw_current_cube_in_draw_map:           
    dec cx ; 0-9
    dec dx ; 0-9
    
    push cx
    push dx
    call getNumInGridByRowAndCol  
    mov bx, RetNumByRowAndColInGrid
	
    cmp CubeOpenCloseGrid[bx], CUBE_IS_FLAG  ; If cur cube is flag (so its closed and have array to pixels)
    je draw_flag_in_draw_map  
    
    cmp CubeOpenCloseGrid[bx], CUBE_IS_OPEN  ; If cube is not open
    jne NumGrid_cur_is_not_between_one_to_eight_draw_map   
    
	; If cur cube is open color it gray and then print the number in it
    push cx
    push dx
    push GRAY_COLOR
    call drawCubeAtPos
    
    cmp NumGrid[bx], 1
    je one_draw_map
    cmp NumGrid[bx], 2         
    je two_draw_map    
    cmp NumGrid[bx], 3         
    je three_draw_map
    cmp NumGrid[bx], 4         
    je four_draw_map
    cmp NumGrid[bx], 5         
    je five_draw_map
    cmp NumGrid[bx], 6         
    je six_draw_map
    cmp NumGrid[bx], 7         
    je seven_draw_map    
    cmp NumGrid[bx], 8         
    je eight_draw_map 
    jmp NumGrid_cur_is_not_between_one_to_eight_draw_map
    
one_draw_map:           
    lea di, NumberOne2d                 
    push di           
    jmp end_choose_number_draw_map
two_draw_map:        
    lea di, NumberTwo2d                 
    push di      
    jmp end_choose_number_draw_map
three_draw_map:                   
    lea di, NumberThree2d 
    push di                   
    jmp end_choose_number_draw_map
four_draw_map:                    
    lea di, NumberFour2d 
    push di                  
    jmp end_choose_number_draw_map 
five_draw_map:                     
    lea di, NumberFive2d  
    push di                  
    jmp end_choose_number_draw_map  
six_draw_map:                        
    lea di, NumberSix2d  
    push di                  
    jmp end_choose_number_draw_map 
seven_draw_map:                      
    lea di, NumberSeven2d  
    push di                  
    jmp end_choose_number_draw_map  
eight_draw_map:                       
    lea di, NumberEight2d  
    push di                   
    jmp end_choose_number_draw_map 
 end_choose_number_draw_map:    
    
    push cx
    push dx
    call arrayToPixels  
    
    inc cx
    inc dx 
    jmp end_draw_current_cube_in_draw_map 
	
  NumGrid_cur_is_not_between_one_to_eight_draw_map:  
    cmp CubeOpenCloseGrid[bx], CUBE_IS_OPEN
    je draw_custom_1_open_cube_draw_map
 
    cmp si, 0  ; Check if we should color the cube dark or light
    je light_color_for_cube_draw_map
    jmp dark_color_for_cube_draw_map  
 
  light_color_for_cube_draw_map: 
    mov ax, LIGHT_COLOR  ; Draw light color
	push cx
	push dx
	push ax
	call drawCubeAtPos
    jmp end_color_choose_for_cube_draw_map
          
  dark_color_for_cube_draw_map:
    mov ax, DARK_COLOR  ; Draw dark color
	push cx
	push dx
	push ax
	call drawCubeAtPos								
    jmp end_color_choose_for_cube_draw_map  
    
  draw_custom_1_open_cube_draw_map:               
    mov ax, 07h  ; Draw color gray cube
    cmp NumGrid[bx], 0              
    je end_color_choose_for_cube_draw_map  
    jmp end_draw_current_cube_in_draw_map
	
  draw_flag_in_draw_map: 
  push si
    lea si, Flag2DArr     
    push si  ; Draw flag
    push cx
    push dx
    call arrayToPixels   
    pop si          
    inc cx
    inc dx                    
    jmp end_draw_current_cube_in_draw_map 
	
  end_color_choose_for_cube_draw_map:
    inc cx
    inc dx              
	
  end_draw_current_cube_in_draw_map:  
    pop di
    pop si
    pop cx  
    cmp si, 0
    je si_zero_to_one_outter_loop_draw_map 
    mov si, 0
    jmp end_change_sec_si_draw_map
  si_zero_to_one_outter_loop_draw_map:
    mov si, 1  
  end_change_sec_si_draw_map:
    loop start_inner_loop_in_draw_map   
    pop cx   
    loop start_outter_loop_in_draw_map  
    popa
    ret
endp drawMap

; Scanners & Recursive ======================================================================================================================================================================== Scanners & Recursive


; This proc helps scanNumbers proc by scaninig specific cube in some main cube radius, it gets parameters in bx and dx
proc scanCurCubeForScanNumbers
	pusha
    push bx
    push dx 
    call getNumInGridByRowAndCol               
    cmp RetNumByRowAndColInGrid, OUT_OF_BOUNDS  ; If its out of bounds or a mine check next else inc cur
    je end_scan_cur_cube_for_scan_numbers              
    mov bx, RetNumByRowAndColInGrid   
    cmp NumGrid[bx], IS_MINE
    je end_scan_cur_cube_for_scan_numbers
    inc NumGrid[bx]   
  end_scan_cur_cube_for_scan_numbers:
	popa
	ret
endp scanCurCubeForScanNumbers


; This proc scans the grid and place numbers in bombs radius
proc scanNumbers
    pusha
    mov cx, GridLineLength ; CX is the index of outter loop
  start_outter_loop_in_scan_numbers:
    mov si, cx ; SI is the index of outter loop
    push cx   
    dec si           
    
    mov cx, GridLineLength  ; CX is the index of inner loop
  start_inner_loop_in_scan_numbers:
    mov di, cx             ; DI is the index of inner loop
    push cx   
    dec di      
    
    push di    ; Col index
    push si    ; Row index
    call getNumInGridByRowAndCol     
    
    mov bx, RetNumByRowAndColInGrid
    cmp NumGrid[bx], IS_MINE  ; If cur cube is not a mine skip the number placing
    jne end_inner_loop_in_scan_numbers  
    
    mov bx, di  ; Col index    
    mov dx, si  ; Row index
                  
	; Move to the top left cube in the radius of cur cube
    dec dx
    dec bx
    call scanCurCubeForScanNumbers 
	; Move to the top middle cube in the radius of cur cube
    inc bx  
    call scanCurCubeForScanNumbers
	; Move to the top right cube in the radius of cur cube             
    inc bx
    call scanCurCubeForScanNumbers     
	; Move to the middle left cube in the radius of cur cube       
    sub bx, 2
    inc dx
    call scanCurCubeForScanNumbers
	; Move to the middle right cube in the radius of cur cube                       
    add bx, 2
    call scanCurCubeForScanNumbers
	; Move to the bottom left cube in the radius of cur cube
    sub bx, 2
    inc dx  
    call scanCurCubeForScanNumbers
	; Move to the bottom middle cube in the radius of cur cube          
    inc bx    
    call scanCurCubeForScanNumbers        
	; Move to the bottom right cube in the radius of cur cube          
    inc bx    
    call scanCurCubeForScanNumbers
  end_inner_loop_in_scan_numbers:           
    pop cx
    loop start_inner_loop_in_scan_numbers
    pop cx
    loop start_outter_loop_in_scan_numbers   
    popa
    ret
endp scanNumbers
             
; This proc helps openEmptyCubeRecursive to open recursivly some current cube, it takes ax and bx
proc scanCurCubeForOpenEmptyCubeRecursive
	pusha
	
    push bx
    push ax
    call getNumInGridByRowAndCol    
    ; Check if out of bounds
    cmp RetNumByRowAndColInGrid, OUT_OF_BOUNDS
    je end_scan_cur_cube_for_open_recursive        
    push RetNumByRowAndColInGrid  ; Call this proc (recursivly)
    call openEmptyCubeRecursive
  end_scan_cur_cube_for_open_recursive:
	popa
	ret
endp scanCurCubeForOpenEmptyCubeRecursive
       
; This proc recursivly open all empty cubes in the radius of the cur cube
CUR_CUBE_OPEN_EMPTY_RECURSIVE_GRID_NUM equ bp+4  ; Current cube to open       
proc openEmptyCubeRecursive       
    push bp
    mov bp, sp
    pusha
    
    mov di, [CUR_CUBE_OPEN_EMPTY_RECURSIVE_GRID_NUM]   ;cur cube index
    
    cmp CubeOpenCloseGrid[di], CUBE_IS_OPEN  ;If the cube is already open there is no need of the rest of the proc
    je end_proc_open_CUBE_IS_EMPTY_recursive  
    
    cmp NumGrid[di], IS_MINE  ;If its mine it should not be open
    je end_proc_open_CUBE_IS_EMPTY_recursive
    
    mov CubeOpenCloseGrid[di], CUBE_IS_OPEN  ; Open cur cube
    cmp NumGrid[di], CUBE_IS_EMPTY  ; If the cube is not empty open cur cube but don't open the cubes in the radius
    jne end_proc_open_CUBE_IS_EMPTY_recursive
      
    push di  ; Get row and col by di (cur cube index)
    call getRowAndColByNumInGrid  
    
    mov di, RetRowByNumInGrid  ; di is row index
    mov si, RetColByNumInGrid  ; si is col index
    
    mov ax, di ; ax is row index
    mov bx, si  ; bx is col index
    
	; Move to the top left cube in the radius of cur cube
    dec ax
    dec bx 
	call scanCurCubeForOpenEmptyCubeRecursive
	; Move to the top middle cube in the radius of cur cube      
    inc bx 
	call scanCurCubeForOpenEmptyCubeRecursive
	; Move to the top right cube in the radius of cur cube
    inc bx 
	call scanCurCubeForOpenEmptyCubeRecursive
	; Move to the middle left cube in the radius of cur cube
    sub bx, 2
    inc ax
	call scanCurCubeForOpenEmptyCubeRecursive
	; Move to the middle right cube in the radius of cur cube
    add bx, 2
	call scanCurCubeForOpenEmptyCubeRecursive
	; Move to the bottom left cube in the radius of cur cube
    sub bx, 2
    inc ax   
	call scanCurCubeForOpenEmptyCubeRecursive
	; Move to the bottom middle cube in the radius of cur cube
    inc bx
	call scanCurCubeForOpenEmptyCubeRecursive 
	; Move to the bottom right cube in the radius of cur cube
    inc bx
	call scanCurCubeForOpenEmptyCubeRecursive
	
    end_proc_open_CUBE_IS_EMPTY_recursive:
    popa      
    pop bp
    ret 2
endp openEmptyCubeRecursive 


; This proc checks if the player won
proc playerWonCheck
	
	mov PlayerWon, PLAYER_WON
	mov cx, GridLength
  player_won_check_loop:
	push cx  
	dec cx
	mov bx, cx
	
	
	cmp NumGrid[bx], IS_MINE  ; Not Mine
	je end_player_won_check_loop
	cmp CubeOpenCloseGrid[bx], CUBE_IS_OPEN  ; Closed
	je end_player_won_check_loop
	
	mov PlayerWon, PLAYER_NOT_WON ; If there is a closed cube that is not a mine you did not won
	
  end_player_won_check_loop: 
  pop cx
  loop player_won_check_loop	
	ret
endp playerWonCheck

; mathematical procs ============================================================================================================================================= mathematical procs

; This proc gets bx and returning bx as number between 1 and grid length
proc numBetweenOneAndGridLength 
    ; get bx and return bx as number between GridLength to 0
    push ax
    push dx
          
    mov ax, bx  ; Ax will be divided with the value of bx
    mov bx, GridLength  ; bx will be the dividor with value of the grid length
    xor dx, dx
    div bx
    
    mov bx, dx  ; Takes the modulo of the division
    
    cmp bx, 0
    jne generated_is_not_zero
    inc bx
  generated_is_not_zero:    
           
    pop dx
    pop ax
    ret
endp numBetweenOneAndGridLength
    
; This proc generate mines in random indexes in the greed using prime numbers and simple mathematics
proc generateMines
    pusha
    mov ah, 0  ; Takes time and use it as a seed
    int 1ah   
    mov bx, dx 
    shr bx, 8   
    call numBetweenOneAndGridLength
	
    mov cx, NumOfMines
  random_loop_generate_mines: 
    mov ax, bx  ; Use bx as a seed      
    xor dx, dx  ; Multiply seed with a prime number
    mul FirstPrimeForRandom
    xor dx, dx
    push cx
    mov cx, SecondPrimeForRandom ; Takes the modulo of the multiplication with a second prime number
    div cx      
    pop cx
    mov bx, dx
    push bx     
    call numBetweenOneAndGridLength ; The random number (in bx) will be in the range of the grid
	cmp CubeOpenCloseGrid[bx], CUBE_IS_OPEN
	je cur_generated_cube_is_mine_or_open
    cmp NumGrid[bx], IS_MINE ; If its already a mine, do not place a mine there again
    jne cur_generated_cube_is_not_mine
  cur_generated_cube_is_mine_or_open:
    inc cx         
    inc SecondPrimeForRandom  ; It wont be in endless loop
	jmp end_generate_cur_mine
  cur_generated_cube_is_not_mine:    
    mov NumGrid[bx], IS_MINE 
   end_generate_cur_mine:
    pop bx       
    loop random_loop_generate_mines 
    popa
    ret
endp generateMines

; Screen & Numbers printing ============================================================================================================== Screen & Numbers printing

; This proc prints to the screen   
STRING_TO_PRINT_SCREEN equ bp+8
COLOR_1_OF_SCREEN_TO_PRINT EQU bp+6
COLOR_2_OF_SCREEN_TO_PRINT EQU bp+4
proc printScreen
    push bp
    mov bp, sp
    pusha
	mov cx, 0  
	mov ax, 2  ; Hide cursor so it wont disturb the printing
    int 33h  
  start_outter_loop_print_screen:
    push cx
	mov di, cx
	
	cmp di, SCREEN_LINE_HIGH
	jnb choose_color_2_print_screen
	cmp di, SCREEN_LINE_LOW
	jna choose_color_2_print_screen
	mov bx, [COLOR_1_OF_SCREEN_TO_PRINT]
	jmp end_choose_color_print_screen
  choose_color_2_print_screen:
	mov bx, [COLOR_2_OF_SCREEN_TO_PRINT] 
  end_choose_color_print_screen:
	
	mov cx, 0
  start_inner_loop_print_screen:
    push cx
	
	push cx
	push di
	push bx
	call drawPixelAtPos
	
	inc si
	pop cx
	inc cx
	cmp cx, 400
	jne start_inner_loop_print_screen  
	
	pop cx
	inc cx
	cmp cx, 200
	jne start_outter_loop_print_screen 
	
	mov ah, 2  ; Set mouse cursor to line
    mov bh, 0
    mov dh, PRINT_SCREEN_TEXT_LINE
    mov dl, 0 
    int 10h
	
	mov dx, [STRING_TO_PRINT_SCREEN]
    mov ah, 9
    int 21h 
	
    mov ax, 1h  ; Show mouse
    int 33h
     
    popa   
    pop bp
    ret 6
endp printScreen    

; This proc helps printNum proc by printing single given digit
COL_TO_PRINT_DIGIT EQU BP+8  ; Wanted col to print digit
ROW_TO_PRINT_DIGIT EQU BP+6  ; Wanted row to print digit
DIGIT_TO_PRINT EQU BP+4  ; Digit to print
proc printDigit
    push bp
    mov bp,sp
    pusha
	
	mov ah, 2
    mov bh, 0  ; Page number
    mov dh, [ROW_TO_PRINT_DIGIT]
    mov dl, [COL_TO_PRINT_DIGIT]
    int 10h  ; Set cursor
    mov dx, [DIGIT_TO_PRINT]
    mov ah, 2
    int 21h  ; Print digit
	
	popa
    pop bp
    ret 6
endp printDigit

; This proc helps printNum proc by taking a number and return his first digit, return si as the modulo and di as the division
NUM_TO_FIRST_DIGIT_ASCII equ bp+4 ; Given number
proc numToAsciiOfFirstDigit
    push bp
    mov bp,sp
	
	mov ax, [NUM_TO_FIRST_DIGIT_ASCII]
    mov bl, 10  ; Divide by 10
    div bl
    mov dl, al  ; dl now equal to the division
    xor dh, dh
    mov di, dx ; di equal now to division
    mov al, ah ; al now equal to the modulo
    xor ah, ah
    mov si, ax ; si now equal to the modulo
    add si, 48  ; To make digit to ascii
	
	pop bp
	ret 2
endp numToAsciiOfFirstDigit

; This proc get number (under 200) and print his decimal form to the screen
POS_TO_PRINT_NUM equ bp+6  ; Wanted position to print number
NUM_TO_PRINT equ bp+4  ; Number to print
proc printNum
    push bp
    mov bp,sp
    pusha 
	mov di, [NUM_TO_PRINT]
	mov cx, 3
  loop_print_num:
    push cx
    dec cx ; So its start from 0 to 2
	
    push di  ; Convert digit to ascii
    call numToAsciiOfFirstDigit 
	push cx  ; Print digit
	push [POS_TO_PRINT_NUM]
	push si
	call printDigit
	
    pop cx
  loop loop_print_num
  
    popa
    pop bp
    ret 4
endp printNum

; Others ===================================================================================================================================================Others

; This proc will check if the mouse is clicked and if it was clicked it will take the mouse status and change the grids by it
proc waitForMouseClick  
pusha   
    mov ax, 03h   ; Return cx - cursor x pos, dx - cursor y pos, bx - mouse click status
    int 33h
    cmp bx, 0  ; Check if mouse did click any button
    je mouse_did_not_clicked_or_out_of_range_in_wait_for_mouse_click 
    
    push cx  ; Get grid position of mouse
	push dx
	call getGridCubeByPXPY
	
	push RetGXInGetGridCubeByPXPY  ; Check if mouse is out of grid
	push RetGYInGetGridCubeByPXPY
	call getNumInGridByRowAndCol
	cmp RetNumByRowAndColInGrid, -1
	je mouse_did_not_clicked_or_out_of_range_in_wait_for_mouse_click
                             
    cmp bx, 1  ; Left button clicked
    je left_button_pressed_in_wait_for_mouse_click
    cmp bx, 2  ; Right button clicked
    je  right_button_pressed_in_wait_for_mouse_click  
    jmp mouse_did_not_clicked_or_out_of_range_in_wait_for_mouse_click   
    
    
  left_button_pressed_in_wait_for_mouse_click:  ; If left button was clicked
    push cx
    push dx       
    call getGridCubeByPXPY 
                                                                                                                                          
    push RetGXInGetGridCubeByPXPY  ; Get the index in the grid fot current mouse pos
    push RetGYInGetGridCubeByPXPY
    call getNumInGridByRowAndCol  
        
    mov di, RetNumByRowAndColInGrid  ; Change array value  
    cmp CubeOpenCloseGrid[di], CUBE_IS_CLOSED
    jne end_mouse_did_clicked_in_wait_for_mouse_click   
        
    cmp NumGrid[di], IS_MINE 
    jne cur_cube_to_open_is_not_mine_wait_for_mouse_click   
    mov PlayerDied, 1
  cur_cube_to_open_is_not_mine_wait_for_mouse_click:  
                                    
    push di
    call openEmptyCubeRecursive                                  
    jmp end_mouse_did_clicked_in_wait_for_mouse_click  
  right_button_pressed_in_wait_for_mouse_click:  ; if right button was clicked
    push cx
    push dx
    call getGridCubeByPXPY    
        
    push RetGXInGetGridCubeByPXPY  ; Get the index in the grid
    push RetGYInGetGridCubeByPXPY
    call getNumInGridByRowAndCol   
        
    mov di, RetNumByRowAndColInGrid  ; Change array value     
    cmp CubeOpenCloseGrid[di], CUBE_IS_OPEN
    je end_mouse_did_clicked_in_wait_for_mouse_click 
         
    cmp CubeOpenCloseGrid[di], CUBE_IS_FLAG
    jne no_flag_right_click_wait_for_mouse_click
    mov CubeOpenCloseGrid[di], CUBE_IS_CLOSED       
    dec NumOfFlags
    jmp flag_deleted_right_click_wait_for_mouse_click
        
  no_flag_right_click_wait_for_mouse_click:
    mov CubeOpenCloseGrid[di], CUBE_IS_FLAG     
    inc NumOfFlags
  flag_deleted_right_click_wait_for_mouse_click:
  end_mouse_did_clicked_in_wait_for_mouse_click:   
    call drawMap   
	call playerWonCheck
  loop_until_buttons_unpressed:  ; Loop that waits until the player unpress any mouse button so he wont click and drag the mouse
    mov ax, 03h   ; Return cx - cursor x pos, dx - cursor y pos, bx - mouse click status
    int 33h 
    cmp bx, 0
    jne loop_until_buttons_unpressed     
  mouse_did_not_clicked_or_out_of_range_in_wait_for_mouse_click:
popa
ret
endp waitForMouseClick

; This proc is the main loop of the game
proc mainLoop  
    lea si, StartScreenString  ; Print start screen
    push si
    push FIRST_COLOR_SCREEN_START
    push SECOND_COLOR_SCREEN_START
    call printScreen    
	
    mov cx, 1  ; Loop wait for click that starts the game and print map
  wait_for_first_click_in_main_loop:
    mov ax, 03h   ; Return cx - cursor x pos, dx - cursor y pos, bx - mouse click status
    int 33h   
    cmp bx, 0
    jne end_wait_for_first_click_in_main_loop 
    inc cx
    loop wait_for_first_click_in_main_loop 
  end_wait_for_first_click_in_main_loop: 
   
    lea si, GameScreenString  ; Print in-game screen
    push si
    push FIRST_COLOR_SCREEN_GAME
    push SECOND_COLOR_SCREEN_GAME
    call printScreen
	
    call drawMap 
   
  before_wait_for_start_click_in_main_loop:  ; This loop waits for the player to open cube and after that it generates mines (not in the player choosen cube)
    mov cx, 1
  wait_for_start_click_in_main_loop:
    mov ax, 03h   ; Return cx - cursor x pos, dx - cursor y pos, bx - mouse click status
    int 33h   
    cmp bx, 0
    jne end_wait_for_start_click_in_main_loop 
    inc cx
    loop wait_for_start_click_in_main_loop 
  end_wait_for_start_click_in_main_loop: 
  
	mov ax, 03h   ; Return cx - cursor x pos, dx - cursor y pos, bx - mouse click status
    int 33h   
	push cx  ; Get mouse grid position
	push dx
	call getGridCubeByPXPY
	
	push RetGXInGetGridCubeByPXPY  ; Check if mouse out of grid
	push RetGYInGetGridCubeByPXPY
	call getNumInGridByRowAndCol
	cmp RetNumByRowAndColInGrid, -1
	je before_wait_for_start_click_in_main_loop
	
	mov bx, RetNumByRowAndColInGrid  ; Open current cube (not recursivly because mines weren't generated yet)
	mov CubeOpenCloseGrid[bx], CUBE_IS_OPEN
	
    call generateMines  ; Generate mines based on open cube
    call scanNumbers  ; scan for numbers
	                   
	mov CubeOpenCloseGrid[bx], CUBE_IS_CLOSED
	push bx  ; Now open the choosen cube recursivly
	call openEmptyCubeRecursive
	
    mov ah, 2  ; Set mouse cursor to flags points
    mov bh, 0
    mov dh, 1
    mov dl, 0 
    int 10h
	
	lea dx, NumOfFlagsString ; Print string of flags
    mov ah, 9
    int 21h 
	
   
    mov cx, 1 ; This is the main loop that repeats until you win or hit a mine
  main_loop_mouse_click_loop:  
    push 2 ; place to print
    push NumOfFlags
    call printNum
   
    ;runs in endless loop
   
    call waitForMouseClick 
   
    cmp PlayerDied, PLAYER_DIED
    je player_died_main_loop   
    cmp PlayerWon, PLAYER_WON
	je player_won_main_loop
   
    inc cx             
    loop main_loop_mouse_click_loop  
  player_won_main_loop:     
    mov ax, 2  ; Hide cursor so it wont disturb the printing
    int 33h             
    
	lea si, WonScreenString 
    push si
    push FIRST_COLOR_SCREEN_WON
    push SECOND_COLOR_SCREEN_WON
    call printScreen     
    jmp end_main_loop
  player_died_main_loop:    
    mov ax, 2  ; Hide cursor so it wont disturb the printing
    int 33h  
    
    lea si, DiedScreenString 
    push si
    push FIRST_COLOR_SCREEN_DIED
    push SECOND_COLOR_SCREEN_DIED
    call printScreen  
    
  end_main_loop: 
  
    mov ah, 2  ; Set mouse cursor to line
    mov bh, 0
    mov dh, 9
    mov dl, 0 
    int 10h      
    
	
	lea dx, StringToRestartGame
    mov ah, 9
    int 21h            
    
     mov ax, 1h  ; Show mouse
    int 33h
	
	mov cx, 1  ; Loop wait for click that starts the game and print map
  wait_for_last_click_in_main_loop:
    mov ax, 03h   ; Return cx - cursor x pos, dx - cursor y pos, bx - mouse click status
    int 33h   
    cmp bx, 0
    jne end_wait_for_last_click_in_main_loop 
    inc cx
    loop wait_for_last_click_in_main_loop 
  end_wait_for_last_click_in_main_loop: 
	cmp bx, 1  ; if left button pressed restart game, else exit OS
	jne exit_game_main_loop
	call restartGame
	exit_game_main_loop:
	ret
endp mainLoop 

; This proc restart the game, and the important variables
proc restartGame
	mov cx, GridLength
  loop_to_clear_grids:
    push cx
	dec cx
	mov bx, cx
	mov NumGrid[bx], CUBE_IS_EMPTY  ; Reset NumGrid
	mov CubeOpenCloseGrid[bx], CUBE_IS_CLOSED  ; Reset Open Close grid
	pop cx
	loop loop_to_clear_grids
	mov NumOfFlags, 0  ; Reset number of placed flags
	mov PlayerDied, PLAYER_ALIVE  ; Reset player living status
	mov PlayerWon, 0
	call mainLoop
	ret
endp restartGame 

; end procs -----------------------------------------------------------------------------------------------------------------------------
    
start:     
; set segment registers:
    mov ax, data
    mov ds, ax
    mov es, ax

    ;set graphic mode
    mov ah, 0
    mov al, 13h
    int 10h  
    ;set mouse cursor
    mov ax, 1
    int 33h   
	
    call mainLoop  
	
    mov ax, 4c00h ; exit to operating system.
    int 21h    
ends

end start
