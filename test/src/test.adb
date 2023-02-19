with AUnit.Reporter.Text;
with AUnit.Run;
with AUnit.Test_Suites;

with Vectors_Tests;
with Maps_Tests;

procedure Test is
   All_Test_Cases : constant AUnit.Test_Suites.Access_Test_Suite
     := new AUnit.Test_Suites.Test_Suite;
   function All_Tests return AUnit.Test_Suites.Access_Test_Suite
     is (All_Test_Cases);
   procedure Run is new AUnit.Run.Test_Runner (All_Tests);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   AUnit.Test_Suites.Add_Test (All_Test_Cases, Vectors_Tests.Suite);
   AUnit.Test_Suites.Add_Test (All_Test_Cases, Maps_Tests.Suite);
   Run (Reporter);
end Test;
