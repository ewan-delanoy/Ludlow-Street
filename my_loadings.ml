
(*
 #use"/Users/Ewandelanoy/Documents/OCaml/Ordinary/my_loadings.ml";;
*)

#directory"/Users/Ewandelanoy/Documents/OCaml/Ordinary/_build/";;
#directory"/Users/Ewandelanoy/Documents/OCaml/Ordinary/";;
#directory"/Users/Ewandelanoy/Documents/OCaml/Ordinary/Country/Alaska/";;
#directory"/Users/Ewandelanoy/Documents/OCaml/Ordinary/Country/Germany/";;
#directory"/Users/Ewandelanoy/Documents/OCaml/Ordinary/Creators/";;
#directory"/Users/Ewandelanoy/Documents/OCaml/Ordinary/GParser/";;
#directory"/Users/Ewandelanoy/Documents/OCaml/Ordinary/Generic_syntax_types/";;
#directory"/Users/Ewandelanoy/Documents/OCaml/Ordinary/Global_variables/";;
#directory"/Users/Ewandelanoy/Documents/OCaml/Ordinary/Makefile_makers/";;
#directory"/Users/Ewandelanoy/Documents/OCaml/Ordinary/Ocaml_analysis/";;
#directory"/Users/Ewandelanoy/Documents/OCaml/Ordinary/Ordered_Lists/";;
#directory"/Users/Ewandelanoy/Documents/OCaml/Ordinary/Php_analizer/Great_Replacement/";;
#directory"/Users/Ewandelanoy/Documents/OCaml/Ordinary/Php_analizer/HRecognizer/";;
#directory"/Users/Ewandelanoy/Documents/OCaml/Ordinary/Prepare_html/Parametrization_related/";;
#directory"/Users/Ewandelanoy/Documents/OCaml/Ordinary/Prepare_html/Tag_related/";;
#directory"/Users/Ewandelanoy/Documents/OCaml/Ordinary/Preprinters/";;
#directory"/Users/Ewandelanoy/Documents/OCaml/Ordinary/Sudoku/";;
#directory"/Users/Ewandelanoy/Documents/OCaml/Ordinary/Test_directory6/Test_directory7/Test_directory2/";;
#directory"/Users/Ewandelanoy/Documents/OCaml/Ordinary/Text_editing/";;

#load"nums.cma";;
#load"str.cma";;
#load"unix.cma";;






#load"option.cmo";;
#load"image.cmo";;
#load"basic.cmo";;
#load"listennou.cmo";;
#load"ennig.cmo";;
#load"total_ordering.cmo";;
#load"rational.cmo";;
#load"polynomial.cmo";;
#load"ordered.cmo";;
#load"tidel.cmo";;
#load"hall_algorithm.cmo";;
#load"variable.cmo";;
#load"ordered_variable.cmo";;
#load"three_parts.cmo";;
#load"ordered_bare_set.cmo";;
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
#load"min.cmo";;
#load"strung.cmo";;
#load"substring.cmo";;
#load"charset.cmo";;
#load"characters_in_namespace_name.cmo";;
#load"after.cmo";;
#load"unshadowed_appearance.cmo";;
#load"unqsubstr_helper.cmo";;
#load"overwriter.cmo";;
#load"max.cmo";;
#load"unix_command.cmo";;
#load"capitalize_directory_names.cmo";;
#load"tools_for_absolute_path.cmo";;
#load"absolute_path.cmo";;
#load"io.cmo";;
#load"unqsubstr_replace.cmo";;
#load"no_slashes.cmo";;
#load"father_and_son.cmo";;
#load"directory_name.cmo";;
#load"unjoin_path.cmo";;
#load"unix_compliant_filename.cmo";;
#load"unix_compliant.cmo";;
#load"prepared.cmo";;
#load"cull_string.cmo";;
#load"decimal_number.cmo";;
#load"parenthesed_block.cmo";;
#load"has_suspicious_beginning.cmo";;
#load"decompose_into_paragraphs.cmo";;
#load"shorten_paragraphs.cmo";;
#load"chronometer.cmo";;
#load"explicit.cmo";;
#load"more_coherent_pdf.cmo";;
#load"merge_paragraphs.cmo";;
#load"please_test_me.cmo";;
#load"sudoku_fence.cmo";;
#load"subdirectory.cmo";;
#load"strict_or_loose.cmo";;
#load"stabilize.cmo";;
#load"single_copy_task.cmo";;
#load"nonblank.cmo";;
#load"more_unix.cmo";;
#load"industrial_separator.cmo";;
#load"slow_copy_task.cmo";;
#load"separator.cmo";;
#load"sliced_string.cmo";;
#load"relation.cmo";;
#load"simplex_relation.cmo";;
#load"ocaml_ending.cmo";;
#load"naked_module.cmo";;
#load"modularize.cmo";;
#load"rename_file.cmo";;
#load"german_constant.cmo";;
#load"rename_endsubdirectory.cmo";;
#load"relocate_file.cmo";;
#load"path_is_in_directory.cmo";;
#load"half_dressed_module.cmo";;
#load"mlx_ended_absolute_path.cmo";;
#load"outside_comments_and_strings.cmo";;
#load"my_str.cmo";;
#load"my_str_example.cmo";;
#load"look_for_module_names.cmo";;
#load"recently_deleted.cmo";;
#load"recently_created.cmo";;
#load"recently_changed.cmo";;
#load"ocaml_gsyntax_category.cmo";;
#load"ocaml_gsyntax_item.cmo";;
#load"gparser_result.cmo";;
#load"list_with_indices.cmo";;
#load"gparser.cmo";;
#load"gparser_for_ocaml_language.cmo";;
#load"gparser_fun.cmo";;
#load"gparser_ocaml_comment.cmo";;
#load"gparser_house_with_doors.cmo";;
#load"gparser_apply.cmo";;
#load"read_ocaml_files.cmo";;
#load"ocaml_library.cmo";;
#load"modulesystem_data.cmo";;
#load"find_value_definition.cmo";;
#load"ocaml_target.cmo";;
#load"ordered_string.cmo";;
#load"dircopy_diff.cmo";;
#load"german_unregister_outside_file.cmo";;
#load"german_directories.cmo";;
#load"interesting_modules.cmo";;
#load"debugged_name.cmo";;
#load"alaskan_data.cmo";;
#load"german_data.cmo";;
#load"is_an_ending_or_not.cmo";;
#load"preserve_initial_ordering.cmo";;
#load"read_info_on_file_in_system.cmo";;
#load"alaskan_ingredients_for_ocaml_target.cmo";;
#load"alaskan_force_modification_time.cmo";;
#load"alaskan_command_for_ocaml_target.cmo";;
#load"alaskan_make_ocaml_target.cmo";;
#load"german_unregister_module.cmo";;
#load"german_unregister_mlx_file.cmo";;
#load"find_suitable_ending.cmo";;
#load"german_arrange_positions_in_modulesystem.cmo";;
#load"german_modify_modulesystem.cmo";;
#load"alaskan_remove_debuggables.cmo";;
#load"german_start_debugging.cmo";;
#load"abstract_renamer.cmo";;
#load"german_rename_module.cmo";;
#load"german_rename_directory.cmo";;
#load"german_relocate_module.cmo";;
#load"german_register_outside_file.cmo";;
#load"reconstruct_linear_poset.cmo";;
#load"update_ancs_libs_and_dirs_in_modulesystem.cmo";;
#load"german_recompile.cmo";;
#load"german_make_module_optional.cmo";;
#load"my_global_replace.cmo";;
#load"replace_inside.cmo";;
#load"current_date.cmo";;
#load"german_forget_unregistered_file.cmo";;
#load"german_forget_module.cmo";;
#load"german_forget_file.cmo";;
#load"prepare_dircopy_update.cmo";;
#load"german_delchacre_from_scratch.cmo";;
#load"german_created_or_deleted.cmo";;
#load"german_changed.cmo";;
#load"german_backup_target_system.cmo";;
#load"alaskan_write_makefile.cmo";;
#load"alaskan_up_to_date_targets.cmo";;
#load"alaskan_printer_equipped_types.cmo";;
#load"alaskan_save_all.cmo";;
#load"alaskan_arrange_positions_in_modulesystem.cmo";;
#load"alaskan_reposition_module.cmo";;
#load"alaskan_register_mlx_file.cmo";;
#load"alaskan_try_to_register.cmo";;
#load"alaskan_create_target_system.cmo";;
#load"german_wrapper.cmo";;
#load"self_contained_module_copy.cmo";;
#load"retained_or_not.cmo";;
#load"rat_frac.cmo";;
#load"preprinter.cmo";;
#load"preprinter_homomorphism.cmo";;
#load"preprinter_constructor.cmo";;
#load"gather_preprinter_result.cmo";;
#load"html_text_with_tags.cmo";;
#load"html_hedgehog.cmo";;
#load"html_hedgehog_pack.cmo";;
#load"end_reached_in_recursive_cycle.cmo";;
#load"html_parse_text_with_tags.cmo";;
#load"html_print_text_with_tags.cmo";;
#load"html_template.cmo";;
#load"html_param_config.cmo";;
#load"html_parametrized_atom.cmo";;
#load"html_parametrized_text.cmo";;
#load"special_chars_for_hrecognizer_name.cmo";;
#load"atomic_hrecognizer.cmo";;
#load"nonatomic_hrecognizer.cmo";;
#load"hskeleton.cmo";;
#load"hregistrar.cmo";;
#load"check_hrecognizers_disjointness.cmo";;
#load"nspc_detect.cmo";;
#load"nspc_decomposed_form.cmo";;
#load"lines_in_string.cmo";;
#load"nspc_split.cmo";;
#load"marker.cmo";;
#load"put_markers_everywhere.cmo";;
#load"publicize_block.cmo";;
#load"nspc_standardize.cmo";;
#load"nspc_remove.cmo";;
#load"nspc_reaggregate.cmo";;
#load"nspc_inflate.cmo";;
#load"clean_duplicate_uses.cmo";;
#load"clean_empty_namespaces.cmo";;
#load"nspc_expand_inclusion.cmo";;
#load"functionlike_kind.cmo";;
#load"classlike_kind.cmo";;
#load"classlike_item.cmo";;
#load"functionlike_item.cmo";;
#load"merge_nonfunctions.cmo";;
#load"merge_nonclasses.cmo";;
#load"ivy_aware_marker.cmo";;
#load"ivy_aware_kind.cmo";;
#load"ivy_aware_item.cmo";;
#load"ivy_aware_decomposition.cmo";;
#load"ivy_aware_inflate.cmo";;
#load"ivy_aware_deflate.cmo";;
#load"first_pass_parse.cmo";;
#load"classlike_decomposition.cmo";;
#load"functionlike_decomposition.cmo";;
#load"expand_fnctn_call.cmo";;
#load"expand_classnames.cmo";;
#load"overwrite_at_intervals.cmo";;
#load"tidel2.cmo";;
#load"ordered_integer.cmo";;
#load"optimize_polynomial.cmo";;
#load"ocaml_long_name.cmo";;
#load"isolated_occurrences.cmo";;
#load"german_vague_string.cmo";;
#load"rename_value_inside_module.cmo";;
#load"read_needed_ocaml_files.cmo";;
#load"longest_shared_module.cmo";;
#load"find_value_descendants.cmo";;
#load"compute_all_ocaml_items.cmo";;
#load"multiset.cmo";;
#load"more_uchar.cmo";;
#load"martian_partial_ordering.cmo";;
#load"localized_html.cmo";;
#load"list_of_small_primes.cmo";;
#load"gcd.cmo";;
#load"easy_arithmetic.cmo";;
#load"basics_on_small_primes.cmo";;
#load"arithmetic.cmo";;
#load"legendre_symbol.cmo";;
#load"latex_pretty_printing.cmo";;
#load"interpolation.cmo";;
#load"inclusion.cmo";;
#load"gparser_for_c_language.cmo";;
#load"generalizer.cmo";;
#load"associativity.cmo";;
#load"endsubdirectory.cmo";;
#load"double_augmented.cmo";;
#load"copyable_printing.cmo";;
#load"directory_summary.cmo";;
#load"detect_arithmetic_progressions.cmo";;
#load"descriptive_system.cmo";;
#load"debugger.cmo";;
#load"small_int_based_rational.cmo";;
#load"big_int_based_rational.cmo";;
#load"german_values_in_modules.cmo";;
#load"german_update_copied_compiler.cmo";;
#load"german_pervasives.cmo";;
#load"german_make_self_contained_copy.cmo";;
#load"code_namespace.cmo";;
#load"check_ocaml_dircopy.cmo";;



