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

void forward(int token, int vocabulary_len, int context_len, int layer_count,
             int q_head_count, int kv_head_count, int q_head_per_kv_head_count,
             int embedding_dim, int head_dim, int q_dim, int kv_dim,
             int hidden_dim,

             float epsilon,

             float *embedding_weight, float *mha_norm_weight,
             float *mha_q_weight, float *mha_k_weight, float *mha_v_weight,
             float *mha_out_weight, float *ffn_norm_weight,
             float *ffn_fc_weight, float *ffn_up_weight, float *ffn_out_weight,
             float *out_norm_weight, float *out_weight,

             float *k_cache, float *v_cache, float *logits, int pos,
             int logits_count) {
  __reads("embedding_weight ~> Matrix2(vocabulary_len,embedding_dim)");
  __reads("mha_norm_weight ~> Matrix2(layer_count,embedding_dim)");
  __reads("mha_q_weight ~> Matrix4(layer_count,q_head_count, "
          "head_dim,embedding_dim)");
  float *const embedding = MALLOC1(float, embedding_dim);
  float *const mha_norm = MALLOC1(float, embedding_dim);
  float *const mha_q = MALLOC2(float, q_head_count, head_dim);
  float *const mha_score = MALLOC2(float, q_head_count, context_len);
  float *const mha_blend = MALLOC2(float, q_head_count, head_dim);
  float *const mha_att = MALLOC1(float, embedding_dim);
  float *const mha_out = MALLOC1(float, embedding_dim);
  float *const ffn_norm = MALLOC1(float, embedding_dim);
  float *const ffn_fc = MALLOC1(float, hidden_dim);
  float *const ffn_up = MALLOC1(float, hidden_dim);
  float *const ffn_out = MALLOC1(float, embedding_dim);
  // Get embedding representation of each token in the token sequence
  __ghost(assume, " P := in_range(token, 0..vocabulary_len)");

  for (int e = 0; e < embedding_dim; e++) {
    __xwrites("&embedding[MINDEX1(embedding_dim,e)] ~> Cell");
    __GHOST_BEGIN(focus_embedding_weight, ro_matrix2_focus,
                  "embedding_weight,token,e");

    embedding[MINDEX1(embedding_dim, e)] =
        embedding_weight[MINDEX2(vocabulary_len, embedding_dim, token, e)];
    __GHOST_END(focus_embedding_weight);
  }

  // attention rmsnorm
  for (int l = 0; l < layer_count; l++) {
    __xreads(
        "for i1 in 0..embedding_dim -> &mha_norm_weight[MINDEX2(layer_count, "
        "embedding_dim, l,i1)] ~> Cell");
    __xreads("for q in 0..q_head_count -> for h in 0..head_dim -> for e in "
             "0..embedding_dim -> &mha_q_weight[MINDEX4(layer_count, "
             "q_head_count, head_dim, embedding_dim, l, q, h, e)] ~> Cell");

    const __ghost_fn __ghost_pair_1 = __ghost_begin(
        ro_mindex2_unfold_b, "H := fun access -> for i1 in 0..embedding_dim -> "
                             "access(l, i1) ~> Cell, matrix:= mha_norm_weight, "
                             "n1 := layer_count, n2 := embedding_dim");
    rmsnorm(embedding_dim, mha_norm, embedding,
            &mha_norm_weight[MINDEX2(layer_count, embedding_dim, l, 0)],
            epsilon);
    __ghost_end(__ghost_pair_1);
    for (int q = 0; q < q_head_count; q++) {
      __xreads("for h in 0..head_dim -> for e in "
               "0..embedding_dim -> &mha_q_weight[MINDEX4(layer_count, "
               "q_head_count, head_dim, embedding_dim, l, q, h, e)] ~> Cell");
      __xwrites("for i1 in 0..head_dim -> &mha_q[MINDEX2(q_head_count, "
                "head_dim, q, i1)] ~> UninitCell");
      const __ghost_fn __ghost_pair_2 = __ghost_begin(
          mindex2_unfold_b, "H := fun access -> for i1 in 0..head_dim -> "
                               "access(q, i1) ~> UninitCell, matrix:= mha_q, "
                               "n1 := q_head_count, n2 := head_dim");
      matvec(head_dim, embedding_dim,
             &mha_q[MINDEX2(q_head_count, head_dim, q, 0)], mha_norm,
             &mha_q_weight[MINDEX4(layer_count, q_head_count, head_dim,
                                   embedding_dim, l, q, 0, 0)]);
      __ghost_end(__ghost_pair_2);
    }
  }

  free(embedding);
  free(mha_norm);
  free(mha_q);
  free(mha_att);
  free(mha_score);
  free(mha_blend);
  free(mha_out);
  free(ffn_norm);
  free(ffn_fc);
  free(ffn_up);
  free(ffn_out);
}
