#include <math.h>
#include <optitrust.h>

float _powf(float x, float pow) {
  __pure();
  __admitted();
  return powf(x, pow);
}

float _cosf(float x) {
  __pure();
  __admitted();
  return cos(x);
}

float _sinf(float x) {
  __pure();
  __admitted();
  return sin(x);
}

float _sqrtf(float x) {
  __pure();
  __admitted();
  return sqrtf(x);
}

void rope(int col_count, float* x, int pos) {
  __modifies("x ~> Matrix1(col_count)");
  for (int j = 0; j < col_count; j += 2) {
    __strict();
    __smodifies("x ~> Matrix1(col_count)");
    __ghost(assume, "P := is_subrange(j..(j + 1), 0..col_count)");
    const __ghost_fn focus_subrange =
        __ghost_begin(group_focus_subrange, "sub_range := j..(j + 1)");
    float freq = 1.f / _powf(500000.f, (float)j / (float)col_count);
    float val = (float)pos * 2.f;
    float fcr = _cosf(val);
    float fci = _sinf(val);
    __ghost([&]() {
      __consumes("for i1 in j..(j + 1) -> &x[MINDEX1(col_count, i1)] ~> Cell");
      __produces("&x[MINDEX1(col_count, j)] ~> Cell");
      __produces("&x[MINDEX1(col_count, j + 1)] ~> Cell");
      __admitted();
    });
    float v0 = x[MINDEX1(col_count, j)];
    float v1 = x[MINDEX1(col_count, j + 1)];
    x[MINDEX1(col_count, j)] = v0 * fcr - v1 * fci;
    x[MINDEX1(col_count, j + 1)] = v0 * fci + v1 * fcr;
    __ghost([&]() {
      __consumes("&x[MINDEX1(col_count, j)] ~> Cell");
      __consumes("&x[MINDEX1(col_count, j + 1)] ~> Cell");
      __produces("for i1 in j..(j + 1) -> &x[MINDEX1(col_count, i1)] ~> Cell");
      __admitted();
    });
    __ghost_end(focus_subrange);
  }
}

float _expf(float x) {
  __pure();
  __admitted();
  return expf(x);
}

void softmax(int col_count, int col_stride, float* x) {
  __modifies("x ~> Matrix1(col_count)");
  __ghost(assume, "P := in_range(0, 0..col_count)");
  const __ghost_fn max = __ghost_begin(ro_matrix1_focus, "matrix := x, i := 0");
  float max_val = x[MINDEX1(col_count, 0)];
  __ghost_end(max);
  const __ghost_fn focus_subrange =
      __ghost_begin(group_focus_subrange, "sub_range := 1..col_count");
  for (int j = 1; j < col_count; j++) {
    __strict();
    __smodifies(
        "Wand(for i1 in 1..col_count -> &x[MINDEX1(col_count, i1)] ~> Cell, x "
        "~> Matrix1(col_count))");
    __smodifies("&max_val ~> Cell");
    __xmodifies("&x[MINDEX1(col_count, j)] ~> Cell");
    if (x[MINDEX1(col_count, j)] > max_val) {
      max_val = x[MINDEX1(col_count, j)];
    }
  }
  __ghost_end(focus_subrange);
  float sum = 0.f;
  for (int j = 0; j < col_count; j++) {
    __strict();
    __smodifies("&sum ~> Cell");
    __sreads("&max_val ~> Cell");
    __xmodifies("&x[MINDEX1(col_count, j)] ~> Cell");
    x[MINDEX1(col_count, j)] = _expf(x[MINDEX1(col_count, j)] - max_val);
    sum += x[MINDEX1(col_count, j)];
  }
  for (int j = 0; j < col_count; j++) {
    __strict();
    __sreads("&sum ~> Cell");
    __xmodifies("&x[MINDEX1(col_count, j)] ~> Cell");
    x[MINDEX1(col_count, j)] /= sum;
  }
}

void rmsnorm(int col_count, float* y, float* x, float* w, float epsilon) {
  __writes("y ~> Matrix1(col_count)");
  __reads("x ~> Matrix1(col_count)");
  __reads("w ~> Matrix1(col_count)");
  float ss = 0.f;
  for (int j = 0; j < col_count; j++) {
    __strict();
    __smodifies("&ss ~> Cell");
    __xreads("&x[MINDEX1(col_count, j)] ~> Cell");
    ss += x[MINDEX1(col_count, j)] * x[MINDEX1(col_count, j)];
  }
  ss /= (float)col_count;
  ss += epsilon;
  ss = 1.f / _sqrtf(ss);
  for (int j = 0; j < col_count; j++) {
    __strict();
    __sreads("&ss ~> Cell");
    __xwrites("&y[MINDEX1(col_count, j)] ~> Cell");
    __xreads("&x[MINDEX1(col_count, j)] ~> Cell");
    __xreads("&w[MINDEX1(col_count, j)] ~> Cell");
    y[MINDEX1(col_count, j)] =
        w[MINDEX1(col_count, j)] * (ss * x[MINDEX1(col_count, j)]);
  }
}

void matvec(int col_count, int red_count, float* x, float* y, float* w) {
  __writes("x ~> Matrix1(col_count)");
  __reads("y ~> Matrix1(red_count)");
  __reads("w ~> Matrix2(col_count, red_count)");
  for (int j = 0; j < col_count; j++) {
    __strict();
    __sreads("y ~> Matrix1(red_count)");
    __sreads("w ~> Matrix2(col_count, red_count)");
    __xwrites("&x[MINDEX1(col_count, j)] ~> Cell");
    x[MINDEX1(col_count, j)] = 0.f;
    for (int k = 0; k < red_count; k++) {
      __strict();
      __smodifies("&x[MINDEX1(col_count, j)] ~> Cell");
      __sreads("y ~> Matrix1(red_count)");
      __sreads("w ~> Matrix2(col_count, red_count)");
      const __ghost_fn focusy =
          __ghost_begin(ro_matrix1_focus, "matrix := y, i := k");
      const __ghost_fn focusw =
          __ghost_begin(ro_matrix2_focus, "matrix := w, i := j, j := k");
      x[MINDEX1(col_count, j)] +=
          y[MINDEX1(red_count, k)] * w[MINDEX2(col_count, red_count, j, k)];
      __ghost_end(focusy);
      __ghost_end(focusw);
    }
  }
}

void forward(int token, int vocabulary_len, int context_len, int layer_count,
             int q_head_count, int kv_head_count, int q_head_per_kv_head_count,
             int embedding_dim, int head_dim, int q_dim, int kv_dim,
             int hidden_dim, float epsilon, float* embedding_weight,
             float* mha_norm_weight, float* mha_q_weight, float* mha_k_weight,
             float* mha_v_weight, float* mha_out_weight, float* ffn_norm_weight,
             float* ffn_fc_weight, float* ffn_up_weight, float* ffn_out_weight,
             float* out_norm_weight, float* out_weight, float* k_cache,
             float* v_cache, float* logits, int pos, int logits_count) {
  __reads("embedding_weight ~> Matrix2(vocabulary_len, embedding_dim)");
  __reads("mha_norm_weight ~> Matrix2(layer_count, embedding_dim)");
  __reads(
      "mha_q_weight ~> Matrix4(layer_count, q_head_count, head_dim, "
      "embedding_dim)");
  float* const embedding =
      (float*)malloc(MSIZE1(embedding_dim) * sizeof(float));
  float* const mha_norm = (float*)malloc(MSIZE1(embedding_dim) * sizeof(float));
  float* const mha_q =
      (float*)malloc(MSIZE2(q_head_count, head_dim) * sizeof(float));
  __ghost(assume, "P := in_range(token, 0..vocabulary_len)");
  for (int e = 0; e < embedding_dim; e++) {
    __strict();
    __sreads("embedding_weight ~> Matrix2(vocabulary_len, embedding_dim)");
    __xwrites("&embedding[MINDEX1(embedding_dim, e)] ~> Cell");
    const __ghost_fn focus_embedding_weight = __ghost_begin(
        ro_matrix2_focus, "matrix := embedding_weight, i := token, j := e");
    embedding[MINDEX1(embedding_dim, e)] =
        embedding_weight[MINDEX2(vocabulary_len, embedding_dim, token, e)];
    __ghost_end(focus_embedding_weight);
  }
  for (int l = 0; l < layer_count; l++) {
    __strict();
    __smodifies("mha_q ~> UninitMatrix2(q_head_count, head_dim)");
    __smodifies("mha_norm ~> UninitMatrix1(embedding_dim)");
    __sreads("embedding ~> Matrix1(embedding_dim)");
    __xreads(
        "for i1 in 0..embedding_dim -> &mha_norm_weight[MINDEX2(layer_count, "
        "embedding_dim, l, i1)] ~> Cell");
    __xreads(
        "for q in 0..q_head_count -> for h in 0..head_dim -> for e in "
        "0..embedding_dim -> &mha_q_weight[MINDEX4(layer_count, q_head_count, "
        "head_dim, embedding_dim, l, q, h, e)] ~> Cell");
    rmsnorm(embedding_dim, &mha_norm[MINDEX0()], &embedding[MINDEX0()],
            &mha_norm_weight[MINDEX2(layer_count, embedding_dim, l, 0)],
            epsilon);
    for (int q = 0; q < q_head_count; q++) {
      __strict();
      __sreads("&mha_norm[MINDEX0()] ~> Matrix1(embedding_dim)");
      __xmodifies(
          "for i1 in 0..head_dim -> &mha_q[MINDEX2(q_head_count, head_dim, q, "
          "i1)] ~> UninitCell");
      __xreads(
          "for h in 0..head_dim -> for e in 0..embedding_dim -> "
          "&mha_q_weight[MINDEX4(layer_count, q_head_count, head_dim, "
          "embedding_dim, l, q, h, e)] ~> Cell");
      matvec(head_dim, embedding_dim,
             &mha_q[MINDEX2(q_head_count, head_dim, q, 0)],
             &mha_norm[MINDEX0()],
             &mha_q_weight[MINDEX4(layer_count, q_head_count, head_dim,
                                   embedding_dim, l, q, 0, 0)]);
    }
  }
  free(embedding);
  free(mha_norm);
  free(mha_q);
}

void generate_prompt_proc(int vocabulary_len, int context_len, int layer_count,
                          int q_head_count, int kv_head_count,
                          int q_head_per_kv_head_count, int embedding_dim,
                          int head_dim, int q_dim, int kv_dim, int hidden_dim,
                          float epsilon, float* embedding_weight,
                          float* mha_norm_weight, float* mha_q_weight,
                          float* mha_k_weight, float* mha_v_weight,
                          float* mha_out_weight, float* ffn_norm_weight,
                          float* ffn_fc_weight, float* ffn_up_weight,
                          float* ffn_out_weight, float* out_norm_weight,
                          float* out_weight, float* k_cache, float* v_cache,
                          float* logits, int* sequence, int sequence_len) {
  __reads("embedding_weight ~> Matrix2(vocabulary_len, embedding_dim)");
  __reads("mha_norm_weight ~> Matrix2(layer_count, embedding_dim)");
  __reads("sequence ~> Matrix1(sequence_len)");
  __reads(
      "mha_q_weight ~> Matrix4(layer_count, q_head_count, head_dim, "
      "embedding_dim)");
  float* const embedding =
      (float*)malloc(MSIZE2(sequence_len, embedding_dim) * sizeof(float));
  float* const mha_norm =
      (float*)malloc(MSIZE2(sequence_len, embedding_dim) * sizeof(float));
  float* const mha_q = (float*)malloc(
      MSIZE3(sequence_len, q_head_count, head_dim) * sizeof(float));
  for (int i = 0; i < sequence_len; i++) {
    __strict();
    __sreads("embedding_weight ~> Matrix2(vocabulary_len, embedding_dim)");
    __xwrites(
        "for e in 0..embedding_dim -> &embedding[MINDEX2(sequence_len, "
        "embedding_dim, i, e)] ~> Cell");
    __xreads("&sequence[MINDEX1(sequence_len, i)] ~> Cell");
    const int logits_count = 1;
    const int token = sequence[MINDEX1(sequence_len, i)];
    __ghost(assume, "P := in_range(token, 0..vocabulary_len)", "#_8 <- H");
    for (int e = 0; e < embedding_dim; e++) {
      __strict();
      __sreads("embedding_weight ~> Matrix2(vocabulary_len, embedding_dim)");
      __xwrites(
          "&embedding[MINDEX2(sequence_len, embedding_dim, i, e)] ~> Cell");
      const __ghost_fn focus_embedding_weight = __ghost_begin(
          ro_matrix2_focus, "matrix := embedding_weight, i := token, j := e");
      embedding[MINDEX2(sequence_len, embedding_dim, i, e)] =
          embedding_weight[MINDEX2(vocabulary_len, embedding_dim, token, e)];
      __ghost_end(focus_embedding_weight);
    }
  }
  for (int l = 0; l < layer_count; l++) {
    __strict();
    __smodifies("mha_q ~> UninitMatrix3(sequence_len, q_head_count, head_dim)");
    __smodifies("mha_norm ~> UninitMatrix2(sequence_len, embedding_dim)");
    __sreads("embedding ~> Matrix2(sequence_len, embedding_dim)");
    __sreads("mha_norm_weight ~> Matrix2(layer_count, embedding_dim)");
    __sreads(
        "mha_q_weight ~> Matrix4(layer_count, q_head_count, head_dim, "
        "embedding_dim)");
    for (int i = 0; i < sequence_len; i++) {
      __strict();
      __sreads("mha_norm_weight ~> Matrix2(layer_count, embedding_dim)");
      __sreads(
          "mha_q_weight ~> Matrix4(layer_count, q_head_count, head_dim, "
          "embedding_dim)");
      __xmodifies(
          "for i1 in 0..q_head_count -> for i2 in 0..head_dim -> "
          "&mha_q[MINDEX3(sequence_len, q_head_count, head_dim, i, i1, i2)] ~> "
          "UninitCell");
      __xmodifies(
          "for i1 in 0..embedding_dim -> &mha_norm[MINDEX2(sequence_len, "
          "embedding_dim, i, i1)] ~> UninitCell");
      __xreads(
          "for i1 in 0..embedding_dim -> &embedding[MINDEX2(sequence_len, "
          "embedding_dim, i, i1)] ~> Cell");
      const __ghost_fn __ghost_pair_2 = __ghost_begin(
          ro_group_focus,
          "i := l, items := fun (l: int) -> for i1 in 0..embedding_dim -> "
          "&mha_norm_weight[MINDEX2(layer_count, embedding_dim, l, i1)] ~> "
          "Cell");
      const __ghost_fn __ghost_pair_1 = __ghost_begin(
          ro_group_focus,
          "i := l, items := fun (l: int) -> for q in 0..q_head_count -> for h "
          "in 0..head_dim -> for e in 0..embedding_dim -> "
          "&mha_q_weight[MINDEX4(layer_count, q_head_count, head_dim, "
          "embedding_dim, l, q, h, e)] ~> Cell");
      rmsnorm(
          embedding_dim, &mha_norm[MINDEX2(sequence_len, embedding_dim, i, 0)],
          &embedding[MINDEX2(sequence_len, embedding_dim, i, 0)],
          &mha_norm_weight[MINDEX2(layer_count, embedding_dim, l, 0)], epsilon);
      for (int q = 0; q < q_head_count; q++) {
        __strict();
        __sreads(
            "&mha_norm[MINDEX2(sequence_len, embedding_dim, i, 0)] ~> "
            "Matrix1(embedding_dim)");
        __xmodifies(
            "for i1 in 0..head_dim -> &mha_q[MINDEX3(sequence_len, "
            "q_head_count, head_dim, i, q, i1)] ~> UninitCell");
        __xreads(
            "for h in 0..head_dim -> for e in 0..embedding_dim -> "
            "&mha_q_weight[MINDEX4(layer_count, q_head_count, head_dim, "
            "embedding_dim, l, q, h, e)] ~> Cell");
        matvec(head_dim, embedding_dim,
               &mha_q[MINDEX3(sequence_len, q_head_count, head_dim, i, q, 0)],
               &mha_norm[MINDEX2(sequence_len, embedding_dim, i, 0)],
               &mha_q_weight[MINDEX4(layer_count, q_head_count, head_dim,
                                     embedding_dim, l, q, 0, 0)]);
      }
      __ghost_end(__ghost_pair_1);
      __ghost_end(__ghost_pair_2);
    }
  }
  free(mha_q);
  free(mha_norm);
  free(embedding);
}
