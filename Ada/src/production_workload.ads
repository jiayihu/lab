--|---------------------------------------------------------------------------
--|
--| Unit Name:  Workload
--|
--| Unit Type:  Package Specification
--|
--| Description:
--|   Encapsulates the synthetic computational workload of a Hartstone task.
--|   The actual computation is performed by the Small_Whetstone procedure,
--|   a variant of the Whetstone benchmark program.  The amount of work
--|   requested is expressed in thousands of Whetstone instructions, or
--|   Kilo-Whetstones.  An internal consistency check is performed on the
--|   workload computation within Small_Whetstone; if it fails, an exception
--|   is raised.
--|
--|---------------------------------------------------------------------------

package Production_Workload is

  Workload_Failure : exception; -- Raised if Small_Whetstone self-check fails

  --|
  --| Unit Name: Small_Whetstone
  --|
  --| Unit Type: Procedure Specification
  --|
  --| Description:
  --|   Performs the computational workload of a Hartstone task.  The
  --|   computation is a scaled-down version of the one performed by the
  --|   full Whetstone benchmark program.  An exception is raised if the
  --|   computation fails to satisfy an internal consistency check.  This
  --|   procedure does not return any "result" from its computation; its
  --|   sole function is to give a Hartstone task something to do.
  --|
  --| Parameters:
  --|   Kilo_Whets: The number of Kilo-Whetstone instructions to be performed
  --|     by the procedure.  A value of 1 means one thousand Whetstone
  --|     instructions will be executed as the computational load.
  --|

  procedure Small_Whetstone(Kilo_Whets : Positive) with Inline => True;
  -- pragma Inline(Small_Whetstone); -- Some implementations may ignore this

end Production_Workload;
