#*****************************************************************************************
# project.tcl: Tcl script for re-creating project 'spinaltap'
#
# Run as 'vivado -mode batch -source <path>/project.tcl' after running hardware generation
#
#*****************************************************************************************

namespace eval _tcl {
proc get_script_folder {} {
   set script_path [file normalize [info script]]
   set script_folder [file dirname $script_path]
   return $script_folder
}
}
variable script_folder
set script_folder [_tcl::get_script_folder]

# Check file required for this script exists
proc checkRequiredFiles {folder} {
  set status true
  set files [list \
   "$folder/system.tcl" \
   "$folder/constraints.xdc" \
  ]
  foreach ifile $files {
    if { ![file isfile $ifile] } {
      puts " Could not find local file $ifile "
      set status false
    }
  }

  set files [list \
   "$folder/../../ApbSpinalTap.v"
  ]
  foreach ifile $files {
    if { ![file isfile $ifile] } {
      puts " Could not find remote file $ifile "
      set status false
    }
  }

  return $status
}

# allow project name override from TCL console
set project_name "spinaltap"
if { [info exists ::user_project_name] } {
  set project_name $::user_project_name
}

variable script_file
set script_file "project.tcl"

# Help information for this script
proc print_help {} {
  variable script_file
  puts "\nDescription:"
  puts "Recreate a Vivado project from this script. The created project will be"
  puts "functionally equivalent to the original project for which this script was"
  puts "generated. The script contains commands for creating a project, filesets,"
  puts "runs, adding/importing sources and setting properties on various objects.\n"
  puts "Syntax:"
  puts "$script_file"
  puts "$script_file -tclargs \[--project_name <name>\]"
  puts "$script_file -tclargs \[--help\]\n"
  puts "Usage:"
  puts "Name                   Description"
  puts "-------------------------------------------------------------------------"
  puts "\[--project_name <name>\] Create project with the specified name. Default"
  puts "                       name is the name of the project from where this"
  puts "                       script was generated.\n"
  puts "\[--help\]               Print help information for this script"
  puts "-------------------------------------------------------------------------\n"
  exit 0
}

if { $::argc > 0 } {
  for {set i 0} {$i < $::argc} {incr i} {
    set option [string trim [lindex $::argv $i]]
    switch -regexp -- $option {
      "--project_name" { incr i; set project_name [lindex $::argv $i] }
      "--help"         { print_help }
      default {
        if { [regexp {^-} $option] } {
          puts "ERROR: Unknown option '$option' specified, please type '$script_file -tclargs --help' for usage info.\n"
          return 1
        }
      }
    }
  }
}

# Check for paths and files needed for project creation
set validate_required 1
if { $validate_required } {
  if { [checkRequiredFiles $script_folder] } {
    puts "Tcl file $script_file is valid. All files required for project creation is accessible. "
  } else {
    puts "Tcl file $script_file is not valid. Not all files required for project creation is accessible. "
    return
  }
}

# Create project
create_project ${project_name} ./${project_name} -part xc7z020clg400-1

# Set the directory path for the new project
set proj_dir [get_property directory [current_project]]

# Set project properties
set obj [current_project]
set_property -name "board_part" -value "myir.com:mys-7z020:part0:2.1" -objects $obj
set_property -name "default_lib" -value "xil_defaultlib" -objects $obj
set_property -name "enable_vhdl_2008" -value "1" -objects $obj
set_property -name "ip_cache_permissions" -value "read write" -objects $obj
set_property -name "ip_output_repo" -value "$proj_dir/${project_name}.cache/ip" -objects $obj
set_property -name "mem.enable_memory_map_generation" -value "1" -objects $obj
set_property -name "platform.board_id" -value "mys-7z020" -objects $obj
set_property -name "sim.central_dir" -value "$proj_dir/${project_name}.ip_user_files" -objects $obj
set_property -name "sim.ip.auto_export_scripts" -value "1" -objects $obj
set_property -name "simulator_language" -value "Mixed" -objects $obj
set_property -name "xpm_libraries" -value "XPM_CDC XPM_FIFO XPM_MEMORY" -objects $obj

add_files -norecurse -fileset sources_1 $script_folder/../../ApbSpinalTap.v

source $script_folder/system.tcl
generate_target all [get_files system.bd]
set wrapper_path [make_wrapper -fileset sources_1 -files [ get_files -norecurse system.bd] -top]
add_files -norecurse -fileset sources_1 $wrapper_path

add_files -norecurse -fileset constrs_1 $script_folder/constraints.xdc

set_property top system_wrapper [get_filesets sources_1]
set_property top_auto_set 0 [get_filesets sources_1]

update_compile_order -fileset sources_1

start_gui
