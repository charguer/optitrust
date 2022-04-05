

Compilation of the following if-statement using gcc:


if (((ic_x >= ix_min - B && ic_x <= ix_max + B) // line 828 in original source
  || (ix_min == 0 && ic_x >= ncx - B)
  || (ix_max == ncxminusone && ic_x <= B - 1)) && ((ic_y >= iy_min - B && ic_y <= iy_max + B)
  || (iy_min == 0 && ic_y >= ncy - B)
  || (iy_max == ncyminusone && ic_y <= B - 1)) && ((ic_z >= iz_min - B && ic_z <= iz_max + B)
  || (iz_min == 0 && ic_z >= ncz - B)
  || (iz_max == nczminusone && ic_z <= B - 1)))
	bag_push_serial( // line 829
		&(particlesNext[ID_PRIVATE_BAG][i_cell]),
		my_chunk->dx[i],
		my_chunk->dy[i],
		my_chunk->dz[i],
		my_chunk->vx[i],
		my_chunk->vy[i],
		my_chunk->vz[i]);
else
	bag_push_concurrent( // line 831
		&(particlesNext[ID_SHARED_BAG][i_cell]),
		my_chunk->dx[i],
		my_chunk->dy[i],
		my_chunk->dz[i],
		my_chunk->vx[i],
		my_chunk->vy[i],
		my_chunk->vz[i]);



does not appear to share the loads of the chunk data:

//------------------------then branch------------------------------------------

 2740              	.LVL231:
 2741              	.L137: // <------------------.L137
 2742              		.loc 1 829 0 is_stmt 1
 2743 2534 488B4360 		movq	96(%rbx), %rax
 2744 2538 4963FF   		movslq	%r15d, %rdi
 2745 253b 8B75C4   		movl	-60(%rbp), %esi
 2746 253e 48C1E704 		salq	$4, %rdi
 2747 2542 C4C17B10 		vmovsd	10240(%r12), %xmm5
 2747      AC240028
 2747      0000
 2748 254c C4C17B10 		vmovsd	8192(%r12), %xmm4
 2748      A4240020
 2748      0000
 2749 2556 488B00   		movq	(%rax), %rax
 2750 2559 C4C17B10 		vmovsd	6144(%r12), %xmm3
 2750      9C240018
 2750      0000
 2751 2563 C4C17B10 		vmovsd	4096(%r12), %xmm2
 2751      94240010
 2751      0000
 2752 256d C4C17B10 		vmovsd	2048(%r12), %xmm1
 2752      8C240008
 2752      0000
 2753 2577 480338   		addq	(%rax), %rdi
 2754 257a C4C17B10 		vmovsd	(%r12), %xmm0
 2754      0424
 2755 2580 C5F877   		vzeroupper
 2756 2583 E8000000 		call	bag_push_serial // <------------------SERIAL
 2756      00


//------------------------- else branch-----------------------------------------

 2865              	.LVL239:
 2866              	.L139: // <------------------L139
 2867              		.loc 1 831 0 is_stmt 1
 2868 26d2 488B4360 		movq	96(%rbx), %rax
 2869 26d6 4963FF   		movslq	%r15d, %rdi
 2870 26d9 8B75C4   		movl	-60(%rbp), %esi
 2871 26dc 48C1E704 		salq	$4, %rdi
 2872 26e0 C4C17B10 		vmovsd	10240(%r12), %xmm5
 2872      AC240028
 2872      0000
 2873 26ea C4C17B10 		vmovsd	8192(%r12), %xmm4
 2873      A4240020
 2873      0000
 2874 26f4 488B00   		movq	(%rax), %rax
 2875 26f7 C4C17B10 		vmovsd	6144(%r12), %xmm3
 2875      9C240018
 2875      0000
 2876 2701 C4C17B10 		vmovsd	4096(%r12), %xmm2
 2876      94240010
 2876      0000
 2877 270b C4C17B10 		vmovsd	2048(%r12), %xmm1
 2877      8C240008
 2877      0000
 2878 2715 48037808 		addq	8(%rax), %rdi
 2879 2719 C4C17B10 		vmovsd	(%r12), %xmm0
 2879      0424
 2880 271f C5F877   		vzeroupper
 2881 2722 E8000000 		call	bag_push_concurrent // <------------------CONCURRENT
 2881      00
 2882              	.LVL240:
 2883 2727 E95CFEFF 		jmp	.L130
 2883      FF

//------------------------conditional, with jumps to L137 or L139----------------



 2757              	.LVL232:
 2758              	.L130:
 2759 2588 488B5348 		movq	72(%rbx), %rdx
 2760 258c 8B45C0   		movl	-64(%rbp), %eax
 2761              		.loc 1 822 0 discriminator 20
 2762 258f 4983C601 		addq	$1, %r14
 2763              	.LVL233:
 2764              	.LBB112:
 2765              		.loc 1 837 0 discriminator 20
 2766 2593 488B7308 		movq	8(%rbx), %rsi
 2767              		.loc 1 838 0 discriminator 20
 2768 2597 4C8B4B18 		movq	24(%rbx), %r9
 2769 259b 4983C408 		addq	$8, %r12
 2770 259f 488B7B20 		movq	32(%rbx), %rdi
 2771              		.loc 1 839 0 discriminator 20
 2772 25a3 4C8B5B28 		movq	40(%rbx), %r11
 2773 25a7 488B12   		movq	(%rdx), %rdx
 2774 25aa 428D04F8 		leal	(%rax,%r15,8), %eax
 2775 25ae 4C8B5330 		movq	48(%rbx), %r10
 2776 25b2 C4C27D19 		vbroadcastsd	4088(%r12), %ymm1
 2776      8C24F80F
 2776      0000
 2777              		.loc 1 836 0 discriminator 20
 2778 25bc C4C17C5A 		vcvtps2pd	16(%r9), %ymm5
 2778      6910
 2779 25c2 C4C17C5A 		vcvtps2pd	(%r9), %ymm3
 2779      19
 2780 25c7 4898     		cltq
 2781              		.loc 1 838 0 discriminator 20
 2782 25c9 C4C27D19 		vbroadcastsd	2040(%r12), %ymm2
 2782      9424F807
 2782      0000
 2783              		.loc 1 836 0 discriminator 20
 2784 25d3 C5FC5A67 		vcvtps2pd	16(%rdi), %ymm4
 2784      10
 2785 25d8 488D04C2 		leaq	(%rdx,%rax,8), %rax
 2786              	.LVL234:
 2787              		.loc 1 837 0 discriminator 20
 2788 25dc 488B5310 		movq	16(%rbx), %rdx
 2789              		.loc 1 836 0 discriminator 20
 2790 25e0 C4C17C5A 		vcvtps2pd	16(%r10), %ymm0
 2790      4210
 2791              		.loc 1 837 0 discriminator 20
 2792 25e6 C4427D19 		vbroadcastsd	-8(%r12), %ymm9
 2792      4C24F8
 2793              		.loc 1 836 0 discriminator 20
 2794 25ed C4417C5A 		vcvtps2pd	16(%r11), %ymm8
 2794      4310
 2795 25f3 C4E2EDB8 		vfmadd231pd	%ymm4, %ymm2, %ymm5
 2795      EC
 2796 25f8 C5F91078 		vmovupd	32(%rax), %xmm7
 2796      20
 2797 25fd C5FC5A66 		vcvtps2pd	16(%rsi), %ymm4
 2797      10
 2798 2602 C462F5B8 		vfmadd231pd	%ymm0, %ymm1, %ymm8
 2798      C0
 2799 2607 C5FC5A42 		vcvtps2pd	16(%rdx), %ymm0
 2799      10
 2800 260c C4C2DD98 		vfmadd132pd	%ymm9, %ymm4, %ymm0
 2800      C1
 2801 2611 C5D559E0 		vmulpd	%ymm0, %ymm5, %ymm4
 2802 2615 C5F91028 		vmovupd	(%rax), %xmm5
 2803 2619 C57D29C0 		vmovapd	%ymm8, %ymm0
 2804 261d C4C17C5A 		vcvtps2pd	(%r11), %ymm6
 2804      33
 2805 2622 C4E34518 		vinsertf128	$0x1, 48(%rax), %ymm7, %ymm7
 2805      783001
 2806 2629 C4E35518 		vinsertf128	$0x1, 16(%rax), %ymm5, %ymm5
 2806      681001
 2807 2630 C4E2C598 		vfmadd132pd	%ymm4, %ymm7, %ymm0
 2807      C4
 2808 2635 C4C17C5A 		vcvtps2pd	(%r10), %ymm4
 2808      22
 2809 263a C4E2F5B8 		vfmadd231pd	%ymm4, %ymm1, %ymm6
 2809      F4
 2810 263f C5FC5A27 		vcvtps2pd	(%rdi), %ymm4
 2811 2643 C5FC5A0E 		vcvtps2pd	(%rsi), %ymm1
 2812 2647 C4E2EDB8 		vfmadd231pd	%ymm4, %ymm2, %ymm3
 2812      DC
 2813 264c C5FC5A22 		vcvtps2pd	(%rdx), %ymm4
 2814 2650 C4E2B5B8 		vfmadd231pd	%ymm4, %ymm9, %ymm1
 2814      CC
 2815 2655 C5E559D1 		vmulpd	%ymm1, %ymm3, %ymm2
 2816 2659 C5F81140 		vmovups	%xmm0, 32(%rax)
 2816      20
 2817 265e C4E37D19 		vextractf128	$0x1, %ymm0, 48(%rax)
 2817      403001
 2818              	.LVL235:
 2819 2665 C4E2D598 		vfmadd132pd	%ymm2, %ymm5, %ymm6
 2819      F2
 2820 266a C5F81130 		vmovups	%xmm6, (%rax)
 2821 266e C4E37D19 		vextractf128	$0x1, %ymm6, 16(%rax)
 2821      701001
 2822              	.LBE112:
 2823              		.loc 1 822 0 discriminator 20
 2824 2675 49634508 		movslq	8(%r13), %rax
 2825 2679 4C39F0   		cmpq	%r14, %rax
 2826 267c 0F861201 		jbe	.L202
 2826      0000
 2827 2682 488B4338 		movq	56(%rbx), %rax
 2828 2686 8B7B70   		movl	112(%rbx), %edi
 2829 2689 8B736C   		movl	108(%rbx), %esi
 2830              	.LVL236:
 2831              	.L126:
 2832              		.loc 1 823 0
 2833 268c 488B4DB8 		movq	-72(%rbp), %rcx
 2834 2690 4A8D04B0 		leaq	(%rax,%r14,4), %rax
 2835 2694 448B3C08 		movl	(%rax,%rcx), %r15d
 2836              	.LVL237:
 2837              		.loc 1 825 0
 2838 2698 4489F8   		movl	%r15d, %eax
 2839 269b 99       		cltd
 2840 269c F7FF     		idivl	%edi
 2841 269e 99       		cltd
 2842 269f 4189C1   		movl	%eax, %r9d
 2843 26a2 F7FE     		idivl	%esi
 2844              	.LVL238:
 2845              		.loc 1 828 0
 2846 26a4 3B45B4   		cmpl	-76(%rbp), %eax
 2847 26a7 0F8D43FE 		jge	.L128
 2847      FFFF
 2848              	.L129:
 2849              		.loc 1 828 0 is_stmt 0 discriminator 4
 2850 26ad 8B55A0   		movl	-96(%rbp), %edx
 2851 26b0 85D2     		testl	%edx, %edx
 2852 26b2 750E     		jne	.L148
 2853              		.loc 1 828 0 discriminator 5
 2854 26b4 8B4B68   		movl	104(%rbx), %ecx
 2855 26b7 8D51FF   		leal	-1(%rcx), %edx
 2856 26ba 39D0     		cmpl	%edx, %eax
 2857 26bc 0F8D3AFE 		jge	.L146
 2857      FFFF
 2858              	.L148:
 2859              		.loc 1 828 0 discriminator 8
 2860 26c2 8B5590   		movl	-112(%rbp), %edx
 2861 26c5 3955B0   		cmpl	%edx, -80(%rbp)
 2862 26c8 7508     		jne	.L139
 2863 26ca 85C0     		testl	%eax, %eax
 2864 26cc 0F8E2AFE 		jle	.L146
 2864      FFFF
//------------------------------------------------------------------
  // here was the code for the else branch
//------------------------------------------------------------------
 2884              	.LVL241:
 2885 272c 0F1F4000 		.p2align 4,,10
 2886              		.p2align 3
 2887              	.L132:
 2888              		.loc 1 828 0 discriminator 13
 2889 2730 8B4D94   		movl	-108(%rbp), %ecx
 2890 2733 85C9     		testl	%ecx, %ecx
 2891 2735 750C     		jne	.L144
 2892              		.loc 1 828 0 is_stmt 0 discriminator 14
 2893 2737 83EE01   		subl	$1, %esi
 2894 273a 4139F1   		cmpl	%esi, %r9d
 2895 273d 0F8DD4FD 		jge	.L142
 2895      FFFF
 2896              	.L144:
 2897              		.loc 1 828 0 discriminator 17
 2898 2743 8BB570FF 		movl	-144(%rbp), %esi
 2898      FFFF
 2899 2749 3975C8   		cmpl	%esi, -56(%rbp)
 2900 274c 7584     		jne	.L139
 2901 274e 4585C9   		testl	%r9d, %r9d
 2902 2751 0F8F7BFF 		jg	.L139
 2902      FFFF
 2903              		.loc 1 827 0 is_stmt 1
 2904 2757 8B45A8   		movl	-88(%rbp), %eax
 2905              	.LVL242:
 2906 275a 4421F8   		andl	%r15d, %eax
 2907              		.loc 1 828 0
 2908 275d 3B4580   		cmpl	-128(%rbp), %eax
 2909 2760 0F8DC0FD 		jge	.L133
 2909      FFFF
 2910              	.L134:
 2911              		.loc 1 828 0 is_stmt 0 discriminator 21
 2912 2766 8BB574FF 		movl	-140(%rbp), %esi
 2912      FFFF
 2913 276c 85F6     		testl	%esi, %esi
 2914 276e 750B     		jne	.L140
 2915              		.loc 1 828 0 discriminator 22
 2916 2770 83EF01   		subl	$1, %edi
 2917              	.LVL243:
 2918 2773 39F8     		cmpl	%edi, %eax
 2919 2775 0F8DB9FD 		jge	.L137
 2919      FFFF
 2920              	.LVL244:
 2921              	.L140:
 2922              		.loc 1 828 0 discriminator 24
 2923 277b 8B4D88   		movl	-120(%rbp), %ecx
 2924 277e 394DA8   		cmpl	%ecx, -88(%rbp)
 2925 2781 0F854BFF 		jne	.L139
 2925      FFFF
 2926 2787 85C0     		testl	%eax, %eax
 2927 2789 0F8EA5FD 		jle	.L137
 2927      FFFF
 2928 278f E93EFFFF 		jmp	.L139
 2928      FF
 2929              	.L202:
 2930 2794 4D89EC   		movq	%r13, %r12
 2931 2797 C5F877   		vzeroupper
 2932              	.LVL245:
 2933              	.L109: