#include <math.h>
#include <optitrust.h>
// les __ décrivent des contrats mais je ne comprends pas comment on determine à
// quels bouts de code ils s'appliquent.
inline float _powf(float x, float pow) {
  __pure();
  __admitted();
  return powf(x, pow);
}

inline float _cosf(float x) {
  __pure();
  __admitted();
  return cos(x);
}
inline float _sinf(float x) {
  __pure();
  __admitted();
  return sin(x);
}
inline float _sqrtf(float x) {
  __pure();
  __admitted();
  return sqrtf(x);
}
void rope(int col_count, float *x, int pos) {
  __modifies("x ~> Matrix1(col_count)");
  // TODO : __requires("is_power_of_two 1 col_count");
  for (int j = 0; j < col_count; j += 2) {
    //
    __smodifies("x ~> Matrix1(col_count)");
    __ghost(assume, " P := is_subrange(j..j+1, 0..col_count)");
    __GHOST_BEGIN(focus_subrange, group_focus_subrange, "sub_range := j..j+1");
    // __ghost(matrix1_focus,"matrix := x, i:= j");
    float freq = 1.0f / _powf(500000.0f, (float)j / (float)col_count);
    float val = (float)(pos) * 2.0f;
    float fcr = _cosf(val);
    float fci = _sinf(val);
    ///
    __ghost(
        [&] {
          __consumes(
              "for i1 in j..(j + 1) -> &x[MINDEX1(col_count, i1)] ~> Cell");
          __produces("&x[MINDEX1(col_count,j)] ~> Cell");
          __produces("&x[MINDEX1(col_count,j+1)] ~> Cell");
          __admitted();
        },
        "");
    // TODO : matrix focus on get
    float v0 = x[MINDEX1(col_count, j)];
    float v1 = x[MINDEX1(col_count, j + 1)];

    x[MINDEX1(col_count, j)] = v0 * fcr - v1 * fci;
    x[MINDEX1(col_count, j + 1)] = v0 * fci + v1 * fcr;
    ///
    __ghost(
        [&] {
          __consumes("&x[MINDEX1(col_count,j)] ~> Cell");
          __consumes("&x[MINDEX1(col_count,j + 1)] ~> Cell");
          __produces(
              "for i1 in j..(j + 1) -> &x[MINDEX1(col_count, i1)] ~> Cell");
          __admitted();
        },
        "");
    __GHOST_END(focus_subrange);
  }
}
inline float _expf(float x) {
  __pure();
  __admitted();
  return expf(x);
}
void softmax(int col_count, int col_stride, float *x) {
  __modifies("x ~>Matrix1(col_count)");
  // find max value (for numerical stability)
  __ghost(assume, " P := in_range(0, 0..col_count)");
  __GHOST_BEGIN(max, ro_matrix1_focus, "x,0");
  float max_val = x[MINDEX1(col_count, 0)];
  __GHOST_END(max);
  __GHOST_BEGIN(focus_subrange, group_focus_subrange,
                "sub_range := 1..col_count");
  for (int j = 1; j < col_count; j++) {
    __xmodifies("&x[MINDEX1(col_count,j)] ~> Cell");
    if (x[MINDEX1(col_count, j)] > max_val) {
      max_val = x[MINDEX1(col_count, j)];
    }
  }
  __GHOST_END(focus_subrange);
  // exp and sum
  float sum = 0.0f;
  for (int j = 0; j < col_count; j++) {
    __xmodifies("&x[MINDEX1(col_count,j)] ~> Cell");

    x[MINDEX1(col_count, j)] = _expf(x[MINDEX1(col_count, j)] - max_val);
    sum += x[MINDEX1(col_count, j)];
  }
  // normalize
  for (int j = 0; j < col_count; j++) {
    __xmodifies("&x[MINDEX1(col_count,j)] ~> Cell");

    x[MINDEX1(col_count, j)] /= sum;
  }
}

void rmsnorm(int col_count, float *y, float *x, float *w, float epsilon) {
  __writes("y ~> Matrix1(col_count)");
  __reads("x ~>  Matrix1(col_count)");
  __reads("w ~>  Matrix1(col_count)");
  // calculate sum of squares
  float ss = 0.0f;
  for (int j = 0; j < col_count; j++) {
    __xreads("&x[MINDEX1(col_count, j)] ~> Cell");
    __smodifies(
        "&ss ~> Cell"); // Comment ca peut typer alors que j'ai renseigné nul
                        // part le fait que ss était dans la Heap ?
    // __xmodifies("&ss ~> Cell");
    ss += x[MINDEX1(col_count, j)] * x[MINDEX1(col_count, j)];
  }

  ss /= (float)col_count;
  ss += epsilon;
  ss = 1.0f / _sqrtf(ss);

  // normalize and scale
  for (int j = 0; j < col_count; j++) {
    __xreads("&x[MINDEX1(col_count, j)] ~> Cell");
    __xreads("&w[MINDEX1(col_count, j)] ~> Cell");
    __xwrites("&y[MINDEX1(col_count, j)] ~> Cell");
    // __xreads("&ss ~> Cell");
    y[MINDEX1(col_count, j)] =
        w[MINDEX1(col_count, j)] * (ss * x[MINDEX1(col_count, j)]);
  }
}

void matvec(int col_count, int red_count, float *x, float *y, float *w) {
  __writes("x ~> Matrix1(col_count)");
  __reads("y ~> Matrix1(red_count)");
  __reads("w ~> Matrix2(col_count,red_count)");

  for (int j = 0; j < col_count; j++) {
    __xwrites("&x[MINDEX1(col_count,j)] ~> Cell");

    x[MINDEX1(col_count, j)] = 0.f;
    for (int k = 0; k < red_count; k++) {
      //  Pourquoi xreads(w) ne fonctionne pas ?
      __GHOST_BEGIN(focusy, ro_matrix1_focus, "y,k");
      __GHOST_BEGIN(focusw, ro_matrix2_focus, "w,j,k");
      x[MINDEX1(col_count, j)] +=
          y[MINDEX1(red_count, k)] * w[MINDEX2(col_count, red_count, j, k)];
      __GHOST_END(focusy);
      __GHOST_END(focusw);
    }
  }
}
void matmul(int row_count, int col_count, int red_count, float *const x,
            float *const y, float *const w) {
  __writes("x ~> Matrix2(col_count)");
  __reads("y ~> Matrix2(red_count)");
  __reads("w ~> Matrix2(col_count,red_count)");

  for (int i = 0; i < row_count; i++) {
    for (int j = 0; j < col_count; j++) {
      x[MINDEX2(row_count, col_count, i, j)] = 0.f;
      for (int k = 0; k < red_count; k++) {
        x[MINDEX2(row_count, col_count, i, j)] +=
            y[MINDEX2(row_count, red_count, i, k)] *
            w[MINDEX2(col_count, red_count, j, k)];
      }
    }
  }
}
void forward(int token, int vocabulary_len, int context_len, int layer_count,
             int q_head_count, int kv_head_count, int q_head_per_kv_head_count,
             int embedding_dim, int head_dim, int q_dim, int kv_dim,
             int hidden_dim, float epsilon, float *embedding_weight,
             float *mha_norm_weight, float *mha_q_weight, float *mha_k_weight,
             float *mha_v_weight, float *mha_out_weight, float *ffn_norm_weight,
             float *ffn_fc_weight, float *ffn_up_weight, float *ffn_out_weight,
             float *out_norm_weight, float *out_weight, float *k_cache,
             float *v_cache, float *logits, int pos, int logits_count) {
  float *const embedding = MALLOC1(float, embedding_dim);
  float *const mha_norm = MALLOC1(float, embedding_dim);
  float *const mha_q = MALLOC2(float, q_head_count, head_dim);

  // Get embedding representation of each token in the token sequence

  for (int e = 0; e < embedding_dim; e++) {
    embedding[MINDEX1(embedding_dim, e)] =
        embedding_weight[MINDEX2(vocabulary_len, embedding_dim, token, e)];
  }

  // attention rmsnorm
  for (int l = 0; l < layer_count; l++) {

    rmsnorm(embedding_dim, &mha_norm[MINDEX0()], &embedding[MINDEX0()],
            &mha_norm_weight[MINDEX2(layer_count, embedding_dim, l, 0)],
            epsilon);
    for (int q = 0; q < q_head_count; q++) {
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
                          float epsilon, float *embedding_weight,
                          float *mha_norm_weight, float *mha_q_weight,
                          float *mha_k_weight, float *mha_v_weight,
                          float *mha_out_weight, float *ffn_norm_weight,
                          float *ffn_fc_weight, float *ffn_up_weight,
                          float *ffn_out_weight, float *out_norm_weight,
                          float *out_weight, float *k_cache, float *v_cache,
                          float *logits, int *sequence, int sequence_len) {

  for (int i = 0; i < sequence_len; i++) {
    const int logits_count = 1;
    const int token = sequence[MINDEX1(sequence_len, i)];
    forward(token, vocabulary_len, context_len, layer_count, q_head_count,
            kv_head_count, q_head_per_kv_head_count, embedding_dim, head_dim,
            q_dim, kv_dim, hidden_dim, epsilon, embedding_weight,
            mha_norm_weight, mha_q_weight, mha_k_weight, mha_v_weight,
            mha_out_weight, ffn_norm_weight, ffn_fc_weight, ffn_up_weight,
            ffn_out_weight, out_norm_weight, out_weight, k_cache, v_cache,
            logits, i, logits_count);
  }
}
