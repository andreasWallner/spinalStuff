This repository contains gateware modules/test/etc written in SpinalHDL.

Currently the probably biggest part is comprised of "SpinalTap", an PC to
embedded bus interface device that should/will support multiple interfaces
like SPI/ISO7816/I2C but with a focus on feature needed for verification
of embedded software.

To use/build the project use sbt.
Most entrypoints that generate gateware can be found in `src/main/scala/andreasWallner/components.scala`. Run these via sbt (e.g. `sbt runMain andreasWallner.ApbSpinalTap`)
Most modules have unittests for verification, these can be run via `sbt test`

To run more specific tests either limit to the test class name (e.g. `sbt testOnly andreasWallner.spinaltap.SpinalTapTest`) or to a specific test (`sbt testOnly andreasWallner.io.iso7816.TxRxCoreSim -- -z "RX no"`).
To enable full output of stacktraces when necessary run `sbt testOnly ... -- -oF`.

The current target platform of SpinalTap is a Xilinx Zynq, with software to make the
device available via USB. The WIP Zynq firmware can be found in the spinalTapOs repository,
the WIP PC library to interface with the device in the spinalTapLib repository.
