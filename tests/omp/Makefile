TARGET_MAKE_ALL := check

# Usage: make transfo   or   make check
# Example usage for rebuilding just one test: make optitrust && make sequence_sub_out.cpp

TESTS_WORKING := \
	affinity.ml \
	array_sections1.ml \
	array_sections2.ml \
	array_sections3.ml \
	array_sections4.ml \
	async_target1.ml \
	async_target2.ml \
	async_target3.ml \
	async_target4.ml \
	atomic1.ml \
	atomic2.ml \
	atomic3.ml \
	atomic_restrict1.ml \
	atomic_restrict2.ml \
	barrier_regions.ml \
	collapse1.ml \
	collapse2.ml \
	collapse3.ml \
	copyin.ml \
	copyprivate1.ml \
	copyprivate2.ml \
	copyprivate3.ml \
	critical1.ml \
	critical2.ml \
	cpp_reference.ml \
	declare_target1.ml \
	declare_target3.ml \
	declare_target4.ml \
	declare_target5.ml \
	declare_target6.ml \
	default_none.ml \
	device1.ml \
	device2.ml \
	device3.ml \
	doacross1.ml \
	doacross2.ml \
	doacross3.ml \
	doacross4.ml \
	flush_nolist.ml \
	fpriv_sections.ml \
	icv.ml \
	lastprivate.ml \
	linear_in_loop.ml \
	master.ml \
	mem_model1.ml \
	mem_model2.ml \
	mem_model3.ml \
	nested_loop1.ml \
	nested_loop2.ml \
	nesting_restrict1.ml \
	nesting_restrict2.ml \
	nesting_restrict3.ml \
	nesting_restrict4.ml \
	nesting_restrict5.ml \
	nesting_restrict6.ml \
	nowait1.ml \
	nowait2.ml \
	nthrs_dynamic1.ml \
	nthrs_dynamic2.ml \
	nthrs_nesting.ml \
	ordered1.ml \
	ordered2.ml \
	ordered3.ml \
	parallel.ml \
	ploop1.ml \
	private1.ml \
	private2.ml \
	private3.ml \
	psections.ml \
	reduction1.ml \
	reduction2.ml \
	reduction3.ml \
	reduction4.ml \
	simd1.ml \
	simd2.ml \
	simd3.ml \
	simd4.ml \
	simd5.ml \
	simd6.ml \
	simd7.ml \
	simd8.ml \
	single.ml \
	standalone.ml \
	target1.ml \
	target2.ml \
	target3.ml \
	target4.ml \
	target5.ml \
	target6.ml \
	target_data1.ml \
	target_data2.ml \
	target_data3.ml \
	target_data4.ml \
	target_data5.ml \
	target_data6.ml \
	target_data7.ml \
	target_update1.ml \
	target_update2.ml \
	task_dep1.ml \
	task_dep2.ml \
	task_dep3.ml \
	task_dep5.ml \
	tasking3.ml \
	tasking4.ml \
	tasking5.ml \
	tasking6.ml \
	tasking7.ml \
	tasking8.ml \
	tasking9.ml \
	tasking10.ml \
	tasking11.ml \
	tasking12.ml \
	tasking13.ml \
	tasking14.ml \
	taskloop.ml \
	task_priority.ml \
	taskyield.ml \
	teams1.ml \
	teams2.ml \
	teams3.ml \
	teams4.ml \
	teams5.ml \
	teams6.ml \
	threadprivate1.ml \
	threadprivate2.ml \
	worksharing_critical.ml \

TESTS_TOCHECK := \
	taskgroup.ml \
	get_nthrs.ml \

TESTS := $(TESTS_WORKING)

COMPILE := \


include ../Common.Makefile

# LATER: flattening:  for i (for j ...)   ->  for k    // reverse of gridify
# for i { for j = 0; j < nb[i]; j++)  ...  example: sparse matrix
#   compute the prefix-sum array first

# LATER: target to say "first for loop after instruction foo"