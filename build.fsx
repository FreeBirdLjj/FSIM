// include Fake lib
#l @"packages/FAKE/tools"
#r @"packages/FAKE/tools/FAKE.FSharp.Compiler.Service.dll"
#r @"packages/FAKE/tools/Microsoft.Web.XmlTransform.dll"
#r @"packages/FAKE/tools/Newtonsoft.Json.dll"
#r @"packages/FAKE/tools/NuGet.Core.dll"
#r @"packages/FAKE/tools/FakeLib.dll"

open Fake

// Properties
let buildDir = "./build/"

// Targets
Target "Clean"
<| fun () ->
    CleanDir buildDir

Target "Default"
<| fun () ->
    trace "Hello World from FAKE"

Target "main.exe"
<| fun () ->
    [ "src/AtomicRef.fs"
      "src/BlockingQueue.fs"
      "src/DataStructure.fs"
      "src/main.fs" ]
    |> Fake.FscHelper.Compile [ Fake.FscHelper.FscParam.Out (buildDir + "main.exe")
                                Fake.FscHelper.FscParam.Target Fake.FscHelper.TargetType.Exe
                                Fake.FscHelper.FscParam.Platform Fake.FscHelper.PlatformType.AnyCpu
                                Fake.FscHelper.FscParam.Reference "packages/Suave/lib/net40/Suave.dll"
                                Fake.FscHelper.FscParam.StaticLink "Suave"
                              ]

// Dependencies
"Clean"
  ==> "main.exe"

// start build
RunTargetOrDefault "main.exe"
