set_property CFGBVS VCCO [current_design]
set_property CONFIG_VOLTAGE 3.3 [current_design]

create_clock -period 10.000 -name ifclk -waveform {0.000 5.000} [get_ports io_ifclk]
create_clock -period 38.750 -name fxclk -waveform {0.000 19.375} [get_ports io_fxclk]

set_property PACKAGE_PIN P15 [get_ports io_fxclk]
set_property PACKAGE_PIN P17 [get_ports io_ifclk]
set_property PACKAGE_PIN V16 [get_ports io_reset]

set_property PACKAGE_PIN V11 [get_ports io_flaga]
set_property PACKAGE_PIN V14 [get_ports io_flagb]
set_property PACKAGE_PIN T11 [get_ports io_led1_n]
set_property PACKAGE_PIN U12 [get_ports io_pktend_n]
set_property PACKAGE_PIN U13 [get_ports io_sloe_n]
set_property PACKAGE_PIN V12 [get_ports io_slrd_n]
set_property PACKAGE_PIN U11 [get_ports io_slwr_n]
set_property DRIVE 12 [get_ports {io_pktend_n io_slrd_n io_slwr_n}]
set_property SLEW FAST [get_ports {io_pktend_n io_slrd_n io_slwr_n}]

set_property PACKAGE_PIN K17 [get_ports {io_dq[0]}]
set_property PACKAGE_PIN K18 [get_ports {io_dq[1]}]
set_property PACKAGE_PIN L14 [get_ports {io_dq[2]}]
set_property PACKAGE_PIN M14 [get_ports {io_dq[3]}]
set_property PACKAGE_PIN L18 [get_ports {io_dq[4]}]
set_property PACKAGE_PIN M18 [get_ports {io_dq[5]}]
set_property PACKAGE_PIN R12 [get_ports {io_dq[6]}]
set_property PACKAGE_PIN R13 [get_ports {io_dq[7]}]
set_property PACKAGE_PIN M13 [get_ports {io_dq[8]}]
set_property PACKAGE_PIN R18 [get_ports {io_dq[9]}]
set_property PACKAGE_PIN T18 [get_ports {io_dq[10]}]
set_property PACKAGE_PIN N14 [get_ports {io_dq[11]}]
set_property PACKAGE_PIN P14 [get_ports {io_dq[12]}]
set_property PACKAGE_PIN P18 [get_ports {io_dq[13]}]
set_property PACKAGE_PIN M16 [get_ports {io_dq[14]}]
set_property PACKAGE_PIN M17 [get_ports {io_dq[15]}]
set_property DRIVE 4 [get_ports {io_dq[*]}]

# TODO copied from SDK, constraints seem to be the wrong way around
# and do not really match the FX3 datasheet + they are diabled below
# as well...
set_input_delay -clock ifclk -min 0 [get_ports {io_dq[*] io_flag*}]
set_input_delay -clock ifclk -max 3.5 [get_ports {io_dq[*] io_flag*}]
set_output_delay -clock ifclk -min 0 [get_ports {io_slrd_n io_slwr_n io_pktend_n}]
set_output_delay -clock ifclk -max 7 [get_ports {io_slrd_n io_slwr_n io_pktend_n}] 

set_property PACKAGE_PIN V10 [get_ports {io_gpio_n[0]}]
set_property PACKAGE_PIN T14 [get_ports {io_gpio_n[1]}]
set_property PACKAGE_PIN V15 [get_ports {io_gpio_n[2]}]
set_property PACKAGE_PIN R16 [get_ports {io_gpio_n[3]}]
set_property DRIVE 4 [get_ports {io_gpio_n[*]}]
set_property PULLUP true [get_ports {io_gpio_n[*]}]

#set_property PACKAGE_PIN K16 [get_ports {io_A_3_14[0]}]   ;# A3 / K16~IO_25_15
#set_property PACKAGE_PIN K15 [get_ports {io_A_3_14[1]}]   ;# A4 / K15~IO_L24P_T3_RS1_15
#set_property PACKAGE_PIN J15 [get_ports {io_A_3_14[2]}]   ;# A5 / J15~IO_L24N_T3_RS0_15
#set_property PACKAGE_PIN H15 [get_ports {io_A_3_14[3]}]   ;# A6 / H15~IO_L19N_T3_A21_VREF_15
#set_property PACKAGE_PIN J14 [get_ports {io_A_3_14[4]}]   ;# A7 / J14~IO_L19P_T3_A22_15
#set_property PACKAGE_PIN H17 [get_ports {io_A_3_14[5]}]   ;# A8 / H17~IO_L18P_T2_A24_15
#set_property PACKAGE_PIN G17 [get_ports {io_A_3_14[6]}]   ;# A9 / G17~IO_L18N_T2_A23_15
#set_property PACKAGE_PIN G18 [get_ports {io_A_3_14[7]}]   ;# A10 / G18~IO_L22P_T3_A17_15
#set_property PACKAGE_PIN F18 [get_ports {io_A_3_14[8]}]   ;# A11 / F18~IO_L22N_T3_A16_15
#set_property PACKAGE_PIN E18 [get_ports {io_A_3_14[9]}]   ;# A12 / E18~IO_L21P_T3_DQS_15
#set_property PACKAGE_PIN D18 [get_ports {io_A_3_14[10]}]  ;# A13 / D18~IO_L21N_T3_DQS_A18_15
#set_property PACKAGE_PIN G13 [get_ports {io_A_3_14[11]}]  ;# A14 / G13~IO_0_15
#set_property PACKAGE_PIN F13 [get_ports {io_A_18_30[0]}]  ;# A18 / F13~IO_L5P_T0_AD9P_15
#set_property PACKAGE_PIN E16 [get_ports {io_A_18_30[1]}]  ;# A19 / E16~IO_L11N_T1_SRCC_15
#set_property PACKAGE_PIN C17 [get_ports {io_A_18_30[2]}]  ;# A20 / C17~IO_L20N_T3_A19_15
#set_property PACKAGE_PIN A18 [get_ports {io_A_18_30[3]}]  ;# A21 / A18~IO_L10N_T1_AD11N_15
#set_property PACKAGE_PIN C15 [get_ports {io_A_18_30[4]}]  ;# A22 / C15~IO_L12N_T1_MRCC_15
#set_property PACKAGE_PIN B17 [get_ports {io_A_18_30[5]}]  ;# A23 / B17~IO_L7N_T1_AD2N_15
#set_property PACKAGE_PIN C14 [get_ports {io_A_18_30[6]}]  ;# A24 / C14~IO_L1N_T0_AD0N_15
#set_property PACKAGE_PIN D13 [get_ports {io_A_18_30[7]}]  ;# A25 / D13~IO_L6N_T0_VREF_15
#set_property PACKAGE_PIN A16 [get_ports {io_A_18_30[8]}]  ;# A26 / A16~IO_L8N_T1_AD10N_15
#set_property PACKAGE_PIN B14 [get_ports {io_A_18_30[9]}]  ;# A27 / B14~IO_L2N_T0_AD8N_15
#set_property PACKAGE_PIN B12 [get_ports {io_A_18_30[10]}] ;# A28 / B12~IO_L3N_T0_DQS_AD1N_15
#set_property PACKAGE_PIN A14 [get_ports {io_A_18_30[11]}] ;# A29 / A14~IO_L9N_T1_DQS_AD3N_15
#set_property PACKAGE_PIN B11 [get_ports {io_A_18_30[12]}] ;# A30 / B11~IO_L4P_T0_15

#set_property PACKAGE_PIN J18 [get_ports {io_B_3_14[0]}]   ;# B3 / J18~IO_L23N_T3_FWE_B_15
#set_property PACKAGE_PIN J17 [get_ports {io_B_3_14[1]}]   ;# B4 / J17~IO_L23P_T3_FOE_B_15
#set_property PACKAGE_PIN K13 [get_ports {io_B_3_14[2]}]   ;# B5 / K13~IO_L17P_T2_A26_15
#set_property PACKAGE_PIN J13 [get_ports {io_B_3_14[3]}]   ;# B6 / J13~IO_L17N_T2_A25_15
#set_property PACKAGE_PIN H14 [get_ports {io_B_3_14[4]}]   ;# B7 / H14~IO_L15P_T2_DQS_15
#set_property PACKAGE_PIN G14 [get_ports {io_B_3_14[5]}]   ;# B8 / G14~IO_L15N_T2_DQS_ADV_B_15
#set_property PACKAGE_PIN G16 [get_ports {io_B_3_14[6]}]   ;# B9 / G16~IO_L13N_T2_MRCC_15
#set_property PACKAGE_PIN H16 [get_ports {io_B_3_14[7]}]   ;# B10 / H16~IO_L13P_T2_MRCC_15
#set_property PACKAGE_PIN F16 [get_ports {io_B_3_14[8]}]   ;# B11 / F16~IO_L14N_T2_SRCC_15
#set_property PACKAGE_PIN F15 [get_ports {io_B_3_14[9]}]   ;# B12 / F15~IO_L14P_T2_SRCC_15
#set_property PACKAGE_PIN E17 [get_ports {io_B_3_14[10]}]  ;# B13 / E17~IO_L16P_T2_A28_15
#set_property PACKAGE_PIN D17 [get_ports {io_B_3_14[11]}]  ;# B14 / D17~IO_L16N_T2_A27_15
#set_property PACKAGE_PIN F14 [get_ports {io_B_18_30[0]}]  ;# B18 / F14~IO_L5N_T0_AD9N_15
#set_property PACKAGE_PIN E15 [get_ports {io_B_18_30[1]}]  ;# B19 / E15~IO_L11P_T1_SRCC_15
#set_property PACKAGE_PIN C16 [get_ports {io_B_18_30[2]}]  ;# B20 / C16~IO_L20P_T3_A20_15
#set_property PACKAGE_PIN B18 [get_ports {io_B_18_30[3]}]  ;# B21 / B18~IO_L10P_T1_AD11P_15
#set_property PACKAGE_PIN D15 [get_ports {io_B_18_30[4]}]  ;# B22 / D15~IO_L12P_T1_MRCC_15
#set_property PACKAGE_PIN B16 [get_ports {io_B_18_30[5]}]  ;# B23 / B16~IO_L7P_T1_AD2P_15
#set_property PACKAGE_PIN D14 [get_ports {io_B_18_30[6]}]  ;# B24 / D14~IO_L1P_T0_AD0P_15
#set_property PACKAGE_PIN D12 [get_ports {io_B_18_30[7]}]  ;# B25 / D12~IO_L6P_T0_15
#set_property PACKAGE_PIN A15 [get_ports {io_B_18_30[8]}]  ;# B26 / A15~IO_L8P_T1_AD10P_15
#set_property PACKAGE_PIN B13 [get_ports {io_B_18_30[9]}]  ;# B27 / B13~IO_L2P_T0_AD8P_15
#set_property PACKAGE_PIN C12 [get_ports {io_B_18_30[10]}] ;# B28 / C12~IO_L3P_T0_DQS_AD1P_15
#set_property PACKAGE_PIN A13 [get_ports {io_B_18_30[11]}] ;# B29 / A13~IO_L9P_T1_DQS_AD3P_15
#set_property PACKAGE_PIN A11 [get_ports {io_B_18_30[12]}] ;# B30 / A11~IO_L4N_T0_15

#set_property PACKAGE_PIN U9 [get_ports {io_C_3_15[0]}]    ;# C3 / U9~IO_L21P_T3_DQS_34
#set_property PACKAGE_PIN U8 [get_ports {io_C_3_15[1]}]    ;# C4 / U8~IO_25_34
#set_property PACKAGE_PIN U7 [get_ports {io_C_3_15[2]}]    ;# C5 / U7~IO_L22P_T3_34
#set_property PACKAGE_PIN U6 [get_ports {io_C_3_15[3]}]    ;# C6 / U6~IO_L22N_T3_34
#set_property PACKAGE_PIN T8 [get_ports {io_C_3_15[4]}]    ;# C7 / T8~IO_L24N_T3_34
#set_property PACKAGE_PIN R8 [get_ports {io_C_3_15[5]}]    ;# C8 / R8~IO_L24P_T3_34
#set_property PACKAGE_PIN R7 [get_ports {io_C_3_15[6]}]    ;# C9 / R7~IO_L23P_T3_34
#set_property PACKAGE_PIN T6 [get_ports {io_C_3_15[7]}]    ;# C10 / T6~IO_L23N_T3_34
#set_property PACKAGE_PIN R6 [get_ports {io_C_3_15[8]}]    ;# C11 / R6~IO_L19P_T3_34
#set_property PACKAGE_PIN R5 [get_ports {io_C_3_15[9]}]    ;# C12 / R5~IO_L19N_T3_VREF_34
#set_property PACKAGE_PIN V2 [get_ports {io_C_3_15[10]}]   ;# C13 / V2~IO_L9N_T1_DQS_34
#set_property PACKAGE_PIN U2 [get_ports {io_C_3_15[11]}]   ;# C14 / U2~IO_L9P_T1_DQS_34
#set_property PACKAGE_PIN K6 [get_ports {io_C_3_15[12]}]   ;# C15 / K6~IO_0_34
#set_property PACKAGE_PIN N6 [get_ports {io_C_19_30[0]}]   ;# C19 / N6~IO_L18N_T2_34
#set_property PACKAGE_PIN M6 [get_ports {io_C_19_30[1]}]   ;# C20 / M6~IO_L18P_T2_34
#set_property PACKAGE_PIN L6 [get_ports {io_C_19_30[2]}]   ;# C21 / L6~IO_L6P_T0_34
#set_property PACKAGE_PIN L5 [get_ports {io_C_19_30[3]}]   ;# C22 / L5~IO_L6N_T0_VREF_34
#set_property PACKAGE_PIN N4 [get_ports {io_C_19_30[4]}]   ;# C23 / N4~IO_L16N_T2_34
#set_property PACKAGE_PIN M4 [get_ports {io_C_19_30[5]}]   ;# C24 / M4~IO_L16P_T2_34
#set_property PACKAGE_PIN M3 [get_ports {io_C_19_30[6]}]   ;# C25 / M3~IO_L4P_T0_34
#set_property PACKAGE_PIN M2 [get_ports {io_C_19_30[7]}]   ;# C26 / M2~IO_L4N_T0_34
#set_property PACKAGE_PIN K5 [get_ports {io_C_19_30[8]}]   ;# C27 / K5~IO_L5P_T0_34
#set_property PACKAGE_PIN L4 [get_ports {io_C_19_30[9]}]   ;# C28 / L4~IO_L5N_T0_34
#set_property PACKAGE_PIN L3 [get_ports {io_C_19_30[10]}]  ;# C29 / L3~IO_L2N_T0_34
#set_property PACKAGE_PIN K3 [get_ports {io_C_19_30[11]}]  ;# C30 / K3~IO_L2P_T0_34

#set_property PACKAGE_PIN V9 [get_ports {io_D_3_15[0]}]    ;# D3 / V9~IO_L21N_T3_DQS_34
#set_property PACKAGE_PIN V7 [get_ports {io_D_3_15[1]}]    ;# D4 / V7~IO_L20P_T3_34
#set_property PACKAGE_PIN V6 [get_ports {io_D_3_15[2]}]    ;# D5 / V6~IO_L20N_T3_34
#set_property PACKAGE_PIN V5 [get_ports {io_D_3_15[3]}]    ;# D6 / V5~IO_L10P_T1_34
#set_property PACKAGE_PIN V4 [get_ports {io_D_3_15[4]}]    ;# D7 / V4~IO_L10N_T1_34
#set_property PACKAGE_PIN T5 [get_ports {io_D_3_15[5]}]    ;# D8 / T5~IO_L12P_T1_MRCC_34
#set_property PACKAGE_PIN T4 [get_ports {io_D_3_15[6]}]    ;# D9 / T4~IO_L12N_T1_MRCC_34
#set_property PACKAGE_PIN U4 [get_ports {io_D_3_15[7]}]    ;# D10 / U4~IO_L8P_T1_34
#set_property PACKAGE_PIN U3 [get_ports {io_D_3_15[8]}]    ;# D11 / U3~IO_L8N_T1_34
#set_property PACKAGE_PIN V1 [get_ports {io_D_3_15[9]}]    ;# D12 / V1~IO_L7N_T1_34
#set_property PACKAGE_PIN U1 [get_ports {io_D_3_15[10]}]   ;# D13 / U1~IO_L7P_T1_34
#set_property PACKAGE_PIN T3 [get_ports {io_D_3_15[11]}]   ;# D14 / T3~IO_L11N_T1_SRCC_34
#set_property PACKAGE_PIN R3 [get_ports {io_D_3_15[12]}]   ;# D15 / R3~IO_L11P_T1_SRCC_34
#set_property PACKAGE_PIN P5 [get_ports {io_D_19_30[0]}]   ;# D19 / P5~IO_L13N_T2_MRCC_34
#set_property PACKAGE_PIN N5 [get_ports {io_D_19_30[1]}]   ;# D20 / N5~IO_L13P_T2_MRCC_34
#set_property PACKAGE_PIN P4 [get_ports {io_D_19_30[2]}]   ;# D21 / P4~IO_L14P_T2_SRCC_34
#set_property PACKAGE_PIN P3 [get_ports {io_D_19_30[3]}]   ;# D22 / P3~IO_L14N_T2_SRCC_34
#set_property PACKAGE_PIN T1 [get_ports {io_D_19_30[4]}]   ;# D23 / T1~IO_L17N_T2_34
#set_property PACKAGE_PIN R1 [get_ports {io_D_19_30[5]}]   ;# D24 / R1~IO_L17P_T2_34
#set_property PACKAGE_PIN R2 [get_ports {io_D_19_30[6]}]   ;# D25 / R2~IO_L15N_T2_DQS_34
#set_property PACKAGE_PIN P2 [get_ports {io_D_19_30[7]}]   ;# D26 / P2~IO_L15P_T2_DQS_34
#set_property PACKAGE_PIN N2 [get_ports {io_D_19_30[8]}]   ;# D27 / N2~IO_L3P_T0_DQS_34
#set_property PACKAGE_PIN N1 [get_ports {io_D_19_30[9]}]   ;# D28 / N1~IO_L3N_T0_DQS_34
#set_property PACKAGE_PIN M1 [get_ports {io_D_19_30[10]}]  ;# D29 / M1~IO_L1N_T0_34
#set_property PACKAGE_PIN L1 [get_ports {io_D_19_30[11]}]  ;# D30 / L1~IO_L1P_T0_34

set_property PACKAGE_PIN H15 [get_ports {io_leds1[0]}]     ;# A6 / B21~IO_L21P_T3_DQS_16
set_property PACKAGE_PIN J13 [get_ports {io_leds1[1]}]     ;# B6 / A21~IO_L21N_T3_DQS_16
set_property PACKAGE_PIN J14 [get_ports {io_leds1[2]}]     ;# A7 / D20~IO_L19P_T3_16
set_property PACKAGE_PIN H14 [get_ports {io_leds1[3]}]     ;# B7 / C20~IO_L19N_T3_VREF_16
set_property PACKAGE_PIN H17 [get_ports {io_leds1[4]}]     ;# A8 / B20~IO_L16P_T2_16
set_property PACKAGE_PIN G14 [get_ports {io_leds1[5]}]     ;# B8 / A20~IO_L16N_T2_16
set_property PACKAGE_PIN G17 [get_ports {io_leds1[6]}]     ;# A9 / C19~IO_L13N_T2_MRCC_16
set_property PACKAGE_PIN G16 [get_ports {io_leds1[7]}]     ;# B9 / A19~IO_L17N_T2_16
set_property PACKAGE_PIN G18 [get_ports {io_leds1[8]}]     ;# A10 / C18~IO_L13P_T2_MRCC_16
set_property PACKAGE_PIN H16 [get_ports {io_leds1[9]}]     ;# B10 / A18~IO_L17P_T2_16

set_property PACKAGE_PIN U9 [get_ports {io_leds2[0]}]      ;# C3 / AB17~IO_L2N_T0_13
set_property PACKAGE_PIN V9 [get_ports {io_leds2[1]}]      ;# D3 / AB16~IO_L2P_T0_13
set_property PACKAGE_PIN U8 [get_ports {io_leds2[2]}]      ;# C4 / Y16~IO_L1P_T0_13
set_property PACKAGE_PIN V7 [get_ports {io_leds2[3]}]      ;# D4 / AA16~IO_L1N_T0_13
set_property PACKAGE_PIN U7 [get_ports {io_leds2[4]}]      ;# C5 / AA15~IO_L4P_T0_13
set_property PACKAGE_PIN V6 [get_ports {io_leds2[5]}]      ;# D5 / AB15~IO_L4N_T0_13
set_property PACKAGE_PIN U6 [get_ports {io_leds2[6]}]      ;# C6 / Y13~IO_L5P_T0_13
set_property PACKAGE_PIN V5 [get_ports {io_leds2[7]}]      ;# D6 / AA14~IO_L5N_T0_13
set_property PACKAGE_PIN T8 [get_ports {io_leds2[8]}]      ;# C7 / W14~IO_L6P_T0_13
set_property PACKAGE_PIN V4 [get_ports {io_leds2[9]}]      ;# D7 / Y14~IO_L6N_T0_VREF_13

set_property PACKAGE_PIN R8 [get_ports {io_leds3[0]}]      ;# C8 / AA13~IO_L3P_T0_DQS_13
set_property PACKAGE_PIN T5 [get_ports {io_leds3[1]}]      ;# D8 / AB13~IO_L3N_T0_DQS_13
set_property PACKAGE_PIN R7 [get_ports {io_leds3[2]}]      ;# C9 / AB12~IO_L7N_T1_13
set_property PACKAGE_PIN T4 [get_ports {io_leds3[3]}]      ;# D9 / AB11~IO_L7P_T1_13
set_property PACKAGE_PIN T6 [get_ports {io_leds3[4]}]      ;# C10 / W12~IO_L12N_T1_MRCC_13
set_property PACKAGE_PIN U4 [get_ports {io_leds3[5]}]      ;# D10 / W11~IO_L12P_T1_MRCC_13
set_property PACKAGE_PIN R6 [get_ports {io_leds3[6]}]      ;# C11 / AA11~IO_L9N_T1_DQS_13
set_property PACKAGE_PIN U3 [get_ports {io_leds3[7]}]      ;# D11 / AA10~IO_L9P_T1_DQS_13
set_property PACKAGE_PIN R5 [get_ports {io_leds3[8]}]      ;# C12 / AA9~IO_L8P_T1_13
set_property PACKAGE_PIN V1 [get_ports {io_leds3[9]}]      ;# D12 / AB10~IO_L8N_T1_13
set_property DRIVE 12 [get_ports {io_leds1[*] io_leds2[*] io_leds3[*]}]

set_property PACKAGE_PIN F18 [get_ports io_switches[0]]    ;# A11 / B18~IO_L11N_T1_SRCC_16
set_property PACKAGE_PIN F16 [get_ports io_switches[1]]    ;# B11 / D17~IO_L12P_T1_MRCC_16
set_property PACKAGE_PIN E18 [get_ports io_switches[2]]    ;# A12 / B17~IO_L11P_T1_SRCC_16
set_property PACKAGE_PIN F15 [get_ports io_switches[3]]    ;# B12 / C17~IO_L12N_T1_MRCC_16
set_property PULLUP TRUE [get_ports {io_switches[*]}]

set_property IOSTANDARD LVCMOS33 [get_ports {io_*}]

# TODO investigate - copied from board SDK, but disables all checks on the output/input
# timings, making timing constraints above useless?
set_false_path -from [get_clocks *mmce_CLKOUT0] -to [get_clocks ]

set_property BITSTREAM.CONFIG.CONFIGRATE 66 [current_design]  
set_property BITSTREAM.CONFIG.SPI_32BIT_ADDR No [current_design]
set_property BITSTREAM.CONFIG.SPI_BUSWIDTH 2 [current_design]
set_property BITSTREAM.GENERAL.COMPRESS true [current_design] 
