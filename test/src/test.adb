with AUnit.Reporter.Text;
with AUnit.Run;

with Vectors_Tests;

procedure Test is
   procedure Run is new AUnit.Run.Test_Runner (Vectors_Tests.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run (Reporter);
end Test;
