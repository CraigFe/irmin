let () =
  Fmt.pr "Running the test suites\n%!";
  Test_http.(with_server servers) (fun () ->
      let tests =
        [ Test_fs.tests;
          Test_chunk.tests;
          Test_core.tests;
          Test_mem.tests;
          Test_http.tests;
          Test_git.tests;
          Test_pack.tests
        ]
      in
      List.iter (fun t -> Alcotest.run ~and_exit:false "irmin" t) tests)

(* Alcotest.run "irmin-debug" (List.flatten tests)) *)
