
(*
 #use"/Users/Ewandelanoy/Documents/OCaml/Ordinary/my_loadings.ml";;
*)

#directory"/Users/Ewandelanoy/Documents/OCaml/Ordinary/";;
#directory"/Users/Ewandelanoy/Documents/OCaml/Ordinary/Country/Alaska/";;
#directory"/Users/Ewandelanoy/Documents/OCaml/Ordinary/Country/Germany/";;
#directory"/Users/Ewandelanoy/Documents/OCaml/Ordinary/Creators/";;
#directory"/Users/Ewandelanoy/Documents/OCaml/Ordinary/GParser/";;
#directory"/Users/Ewandelanoy/Documents/OCaml/Ordinary/Generic_syntax_types/";;
#directory"/Users/Ewandelanoy/Documents/OCaml/Ordinary/Global_variables/";;
#directory"/Users/Ewandelanoy/Documents/OCaml/Ordinary/Makefile_makers/";;
#directory"/Users/Ewandelanoy/Documents/OCaml/Ordinary/Ocaml_analysis/";;
#directory"/Users/Ewandelanoy/Documents/OCaml/Ordinary/Optional/";;
#directory"/Users/Ewandelanoy/Documents/OCaml/Ordinary/Ordered_Lists/";;
#directory"/Users/Ewandelanoy/Documents/OCaml/Ordinary/Php_analizer/";;
#directory"/Users/Ewandelanoy/Documents/OCaml/Ordinary/Php_analizer/Beavers/";;
#directory"/Users/Ewandelanoy/Documents/OCaml/Ordinary/Php_analizer/Php_syntax_types/";;
#directory"/Users/Ewandelanoy/Documents/OCaml/Ordinary/Preprinters/";;
#directory"/Users/Ewandelanoy/Documents/OCaml/Ordinary/Test_directory6/Test_directory7/Test_directory2/";;

#load"nums.cma";;
#load"str.cma";;
#load"unix.cma";;






#load"total_ordering.cmo";;
#load"image.cmo";;
#load"basic.cmo";;
#load"listennou.cmo";;
#load"ordered.cmo";;
#load"Ordered_Lists/tidel.cmo";;
#load"variable.cmo";;
#load"option.cmo";;
#load"hall_algorithm.cmo";;
#load"ennig.cmo";;
#load"sliced_string.cmo";;
#load"rational.cmo";;
#load"polynomial.cmo";;
#load"Ordered_Lists/ordered_variable.cmo";;
#load"three_parts.cmo";;
#load"Ordered_Lists/ordered_bare_set.cmo";;
#load"hidden_vector.cmo";;
#load"vector.cmo";;
#load"cartesian.cmo";;
#load"uple.cmo";;
#load"memoized.cmo";;
#load"int_uple.cmo";;
#load"binary_constraint.cmo";;
#load"van_der_waerden_basics.cmo";;
#load"uutf.cmo";;
#load"utf_eight.cmo";;
#load"no_slashes.cmo";;
#load"father_and_son.cmo";;
#load"capitalize_directory_names.cmo";;
#load"tools_for_absolute_path.cmo";;
#load"directory_name.cmo";;
#load"absolute_path.cmo";;
#load"unjoin_path.cmo";;
#load"unix_compliant_filename.cmo";;
#load"Test_directory6/Test_directory7/Test_directory2/please_test_me.cmo";;
#load"substring.cmo";;
#load"prepared.cmo";;
#load"cull_string.cmo";;
#load"decimal_number.cmo";;
#load"parenthesed_block.cmo";;
#load"subdirectory.cmo";;
#load"strung.cmo";;
#load"strict_or_loose.cmo";;
#load"stabilize.cmo";;
#load"charset.cmo";;
#load"single_copy_task.cmo";;
#load"nonblank.cmo";;
#load"chronometer.cmo";;
#load"explicit.cmo";;
#load"more_unix.cmo";;
#load"io.cmo";;
#load"industrial_separator.cmo";;
#load"slow_copy_task.cmo";;
#load"relation.cmo";;
#load"simplex_relation.cmo";;
#load"overwriter.cmo";;
#load"my_global_replace.cmo";;
#load"replace_inside.cmo";;
#load"rename_file.cmo";;
#load"Makefile_makers/shell_command.cmo";;
#load"Country/Germany/german_constant.cmo";;
#load"rename_endsubdirectory.cmo";;
#load"relocate_file.cmo";;
#load"reconstruct_linear_poset.cmo";;
#load"recently_deleted.cmo";;
#load"recently_created.cmo";;
#load"recently_changed.cmo";;
#load"rat_frac.cmo";;
#load"is_an_ending_or_not.cmo";;
#load"preserve_initial_ordering.cmo";;
#load"Preprinters/preprinter.cmo";;
#load"Preprinters/preprinter_homomorphism.cmo";;
#load"Preprinters/preprinter_constructor.cmo";;
#load"Preprinters/gather_preprinter_result.cmo";;
#load"Ordered_Lists/ordered_string.cmo";;
#load"dircopy_diff.cmo";;
#load"prepare_dircopy_update.cmo";;
#load"Php_analizer/php_punctuator.cmo";;
#load"Generic_syntax_types/associativity.cmo";;
#load"Php_analizer/php_operator.cmo";;
#load"Php_analizer/php_keyword.cmo";;
#load"Generic_syntax_types/token_category.cmo";;
#load"Php_analizer/php_constant_token.cmo";;
#load"Php_analizer/php_token.cmo";;
#load"Php_analizer/positioned_php_token.cmo";;
#load"Php_analizer/positioned_php_token_list.cmo";;
#load"Php_analizer/php_char_range.cmo";;
#load"Php_analizer/php_parser.cmo";;
#load"Php_analizer/php_lexer.cmo";;
#load"Php_analizer/php_recognizer.cmo";;
#load"Php_analizer/php_blocker.cmo";;
#load"Php_analizer/extract_left_block.cmo";;
#load"Php_analizer/php_recognize_block.cmo";;
#load"Php_analizer/php_atomic_selector.cmo";;
#load"Ordered_Lists/tidel2.cmo";;
#load"Php_analizer/php_short_selector.cmo";;
#load"Generic_syntax_types/generalizer.cmo";;
#load"Php_analizer/php_recognizer_homomorphism.cmo";;
#load"Php_analizer/php_constructible_recognizer.cmo";;
#load"glued_or_not.cmo";;
#load"Php_analizer/termite.cmo";;
#load"Php_analizer/termite_reverse_parse.cmo";;
#load"Php_analizer/Php_syntax_types/php_yuze_modifier.cmo";;
#load"Php_analizer/Php_syntax_types/php_script_includer.cmo";;
#load"Php_analizer/Php_syntax_types/php_class_modifier.cmo";;
#load"Php_analizer/php_parser_homomorphism.cmo";;
#load"Php_analizer/php_check_recognizer_rules.cmo";;
#load"Php_analizer/manage_lexed_data.cmo";;
#load"max.cmo";;
#load"dictionary_order.cmo";;
#load"Php_analizer/Beavers/beaver_for_statement.cmo";;
#load"Php_analizer/level_one.cmo";;
#load"path_is_in_directory.cmo";;
#load"overwrite_at_intervals.cmo";;
#load"outside_comments_and_strings.cmo";;
#load"Ordered_Lists/ordered_integer.cmo";;
#load"min.cmo";;
#load"descriptive_system.cmo";;
#load"gcd.cmo";;
#load"detect_arithmetic_progressions.cmo";;
#load"latex_pretty_printing.cmo";;
#load"optimize_polynomial.cmo";;
#load"ocaml_long_name.cmo";;
#load"ocaml_ending.cmo";;
#load"Ocaml_analysis/ocaml_gsyntax_category.cmo";;
#load"Ocaml_analysis/ocaml_gsyntax_item.cmo";;
#load"GParser/gparser_result.cmo";;
#load"list_with_indices.cmo";;
#load"GParser/gparser.cmo";;
#load"GParser/gparser_for_ocaml_language.cmo";;
#load"GParser/gparser_fun.cmo";;
#load"GParser/gparser_ocaml_comment.cmo";;
#load"GParser/gparser_house_with_doors.cmo";;
#load"GParser/gparser_apply.cmo";;
#load"Ocaml_analysis/read_ocaml_files.cmo";;
#load"Makefile_makers/ocaml_library.cmo";;
#load"naked_module.cmo";;
#load"half_dressed_module.cmo";;
#load"Makefile_makers/mlx_ended_absolute_path.cmo";;
#load"Makefile_makers/modulesystem_data.cmo";;
#load"isolated_occurrences.cmo";;
#load"Ocaml_analysis/find_value_definition.cmo";;
#load"Makefile_makers/ocaml_target.cmo";;
#load"Country/Germany/german_unregister_outside_file.cmo";;
#load"Country/Germany/german_directories.cmo";;
#load"interesting_modules.cmo";;
#load"Global_variables/debugger_name.cmo";;
#load"Country/Alaska/alaskan_data.cmo";;
#load"Country/Germany/german_data.cmo";;
#load"my_str.cmo";;
#load"my_str_example.cmo";;
#load"look_for_module_names.cmo";;
#load"Makefile_makers/read_info_on_file_in_system.cmo";;
#load"Country/Alaska/alaskan_ingredients_for_ocaml_target.cmo";;
#load"Country/Alaska/alaskan_force_modification_time.cmo";;
#load"Country/Alaska/alaskan_command_for_ocaml_target.cmo";;
#load"Country/Alaska/alaskan_make_ocaml_target.cmo";;
#load"Country/Germany/german_unregister_module.cmo";;
#load"Country/Germany/german_unregister_mlx_file.cmo";;
#load"find_suitable_ending.cmo";;
#load"Country/Germany/german_arrange_positions_in_modulesystem.cmo";;
#load"Country/Germany/german_modify_modulesystem.cmo";;
#load"Country/Alaska/alaskan_remove_debuggables.cmo";;
#load"Country/Germany/german_start_debugging.cmo";;
#load"Country/Germany/german_write_makefile.cmo";;
#load"Country/Germany/german_up_to_date_targets.cmo";;
#load"Country/Alaska/alaskan_printer_equipped_types.cmo";;
#load"Country/Germany/german_save_all.cmo";;
#load"Makefile_makers/abstract_renamer.cmo";;
#load"Country/Germany/german_rename_module.cmo";;
#load"Country/Germany/german_rename_directory.cmo";;
#load"Country/Germany/german_relocate_module.cmo";;
#load"Country/Germany/german_register_outside_file.cmo";;
#load"Makefile_makers/update_ancs_libs_and_dirs_in_modulesystem.cmo";;
#load"Country/Germany/german_recompile.cmo";;
#load"Country/Germany/german_make_module_optional.cmo";;
#load"current_date.cmo";;
#load"Country/Germany/german_forget_unregistered_file.cmo";;
#load"Country/Germany/german_forget_module.cmo";;
#load"Country/Germany/german_forget_file.cmo";;
#load"Country/Germany/german_delchacre_from_scratch.cmo";;
#load"Country/Germany/german_created_or_deleted.cmo";;
#load"Country/Germany/german_changed.cmo";;
#load"Country/Germany/german_backup_target_system.cmo";;
#load"Country/Alaska/alaskan_arrange_positions_in_modulesystem.cmo";;
#load"Country/Alaska/alaskan_reposition_module.cmo";;
#load"Country/Alaska/alaskan_register_mlx_file.cmo";;
#load"Country/Alaska/alaskan_try_to_register.cmo";;
#load"Country/Alaska/alaskan_create_target_system.cmo";;
#load"Country/Germany/german_wrapper.cmo";;
#load"Country/Germany/german_vague_string.cmo";;
#load"Ocaml_analysis/rename_value_inside_module.cmo";;
#load"Ocaml_analysis/read_needed_ocaml_files.cmo";;
#load"Ocaml_analysis/longest_shared_module.cmo";;
#load"Ocaml_analysis/find_value_descendants.cmo";;
#load"Ocaml_analysis/compute_all_ocaml_items.cmo";;
#load"multiset.cmo";;
#load"modularize.cmo";;
#load"martian_partial_ordering.cmo";;
#load"list_of_small_primes.cmo";;
#load"easy_arithmetic.cmo";;
#load"basics_on_small_primes.cmo";;
#load"arithmetic.cmo";;
#load"legendre_symbol.cmo";;
#load"interpolation.cmo";;
#load"inclusion.cmo";;
#load"GParser/gparser_for_c_language.cmo";;
#load"endsubdirectory.cmo";;
#load"Creators/small_int_based_rational.cmo";;
#load"Creators/big_int_based_rational.cmo";;
#load"Country/Germany/german_values_in_modules.cmo";;
#load"Country/Germany/german_update_copied_compiler.cmo";;
#load"Country/Germany/german_pervasives.cmo";;
#load"check_ocaml_dircopy.cmo";;



