set_property CFGBVS VCCO [current_design]
set_property CONFIG_VOLTAGE 3.3 [current_design]

# http://www.myirtech.com/download/Zynq7000/Z-TURNBOARD_schematic.pdf

set_property PACKAGE_PIN R14 [get_ports io_pwm_0_0]; # RGB LED red
set_property PACKAGE_PIN Y16 [get_ports io_pwm_1_0]; # RGB LED green
set_property PACKAGE_PIN Y17 [get_ports io_pwm_2_0]; # RGB LED blue

set_property PACKAGE_PIN P14 [get_ports {io_port0_0[0]}]; # J6.1
set_property PACKAGE_PIN U15 [get_ports {io_port0_0[1]}]; # J6.3
set_property PACKAGE_PIN W14 [get_ports {io_port0_0[2]}]; # J6.5
set_property PACKAGE_PIN Y14 [get_ports {io_port0_0[3]}]; # J6.7
set_property PACKAGE_PIN C20 [get_ports {io_port0_0[4]}]; # J6.2

set_property PACKAGE_PIN T9  [get_ports {io_port1_0[0]}]; # J7.1
set_property PACKAGE_PIN U10 [get_ports {io_port1_0[1]}]; # J7.3
set_property PACKAGE_PIN Y9  [get_ports {io_port1_0[2]}]; # J7.5
set_property PACKAGE_PIN Y8  [get_ports {io_port1_0[3]}]; # J7.7
set_property PACKAGE_PIN U7  [get_ports {io_port1_0[4]}]; # J7.2

set_property IOSTANDARD LVCMOS33 [get_ports io_*]
