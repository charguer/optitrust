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
    float freq = 1.f / _powf(500000.f, (float)j / (float)col_count);
    float val = (float)pos * 2.f;
    float fcr = _cosf(val);
    float fci = _sinf(val);
    float v0 = x[MINDEX1(col_count, j)];
    float v1 = x[MINDEX1(col_count, j + 1)];
    x[MINDEX1(col_count, j)] = v0 * fcr - v1 * fci;
    x[MINDEX1(col_count, j + 1)] = v0 * fci + v1 * fcr;


  }
}

float _expf(float x) {
  __pure();
  __admitted();
  return expf(x);
}

void softmax(int col_count, int col_stride, float* x) {
  __modifies("x ~> Matrix1(col_count)");
  float max_val = x[MINDEX1(col_count, 0)];
  for (int j = 1; j < col_count; j++) {

    if (x[MINDEX1(col_count, j)] > max_val) {
      max_val = x[MINDEX1(col_count, j)];
    }
  }
  float sum = 0.f;
  for (int j = 0; j < col_count; j++) {
    __xmodifies("&x[MINDEX1(col_count, j)] ~> Cell");
    x[MINDEX1(col_count, j)] = _expf(x[MINDEX1(col_count, j)] - max_val);
    sum += x[MINDEX1(col_count, j)];
  }
  for (int j = 0; j < col_count; j++) {
    __xmodifies("&x[MINDEX1(col_count, j)] ~> Cell");
    x[MINDEX1(col_count, j)] /= sum;
  }
}

void rmsnorm(int col_count, float* y, float* x, float* w, float epsilon) {
  __modifies("y ~> Matrix1(col_count)");
  __reads("x ~> Matrix1(col_count)");
  __reads("w ~> Matrix1(col_count)");
  float ss = 0.f;
  for (int j = 0; j < col_count; j++) {
    __smodifies("&ss ~> Cell");
    const float x_j =x[MINDEX1(col_count, j)];
    ss += x_j * x_j;
  }
  ss /= (float)col_count;
  ss += epsilon;
  ss = 1.f / _sqrtf(ss);
  for (int j = 0; j < col_count; j++) {
    y[MINDEX1(col_count, j)] =
        w[MINDEX1(col_count, j)] * (ss * x[MINDEX1(col_count, j)]);
  }
}

void matvec(int col_count, int red_count, float* x, float* y, float* w) {
  __modifies("x ~> Matrix1(col_count)");
  __reads("y ~> Matrix1(red_count)");
  __reads("w ~> Matrix2(col_count, red_count)");
  for (int j = 0; j < col_count; j++) {
    x[MINDEX1(col_count, j)] = 0.f;
    for (int k = 0; k < red_count; k++) {
      x[MINDEX1(col_count, j)] +=
          y[MINDEX1(red_count, k)] * w[MINDEX2(col_count, red_count, j, k)];
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
  __reads(
      "mha_k_weight ~> Matrix4(layer_count, kv_head_count, head_dim, "
      "embedding_dim)");
  __reads(
      "mha_v_weight ~> Matrix4(layer_count, kv_head_count, head_dim, "
      "embedding_dim)");
  __modifies("k_cache ~> Matrix4(layer_count,kv_head_count, context_len, head_dim)");
  __modifies("v_cache ~> Matrix4(layer_count,kv_head_count, context_len, head_dim)");
  float* const embedding =CALLOC1(float, embedding_dim);
  float* const mha_norm = CALLOC1(float, embedding_dim);
  float* const mha_q = CALLOC2(float, q_head_count, head_dim);

  __ghost(assume, "P := in_range(token, 0..vocabulary_len)");
  for (int e = 0; e < embedding_dim; e++) {
    __xwrites("&embedding[MINDEX1(embedding_dim, e)] ~> Cell");
    embedding[MINDEX1(embedding_dim, e)] =
        embedding_weight[MINDEX2(vocabulary_len, embedding_dim, token, e)];
  }
  __ghost([&]() {
    __consumes("embedding ~> Matrix1(embedding_dim)");
    __consumes("mha_norm ~> UninitMatrix1(embedding_dim)");
    __produces("&embedding[MINDEX0()] ~> Matrix1(embedding_dim)");
    __produces("&mha_norm[MINDEX0()] ~> Matrix1(embedding_dim)");
    __admitted();
  });
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
     for (int h = 0; h < kv_head_count; h++) {
      matvec(head_dim, embedding_dim,
             &k_cache[MINDEX4(layer_count, kv_head_count, context_len, head_dim,
                              l, h, pos, 0)],
             &mha_norm[MINDEX0()],
             &mha_k_weight[MINDEX4(layer_count, kv_head_count, head_dim,
                                   embedding_dim, l, h, 0, 0)]);
    }

    for (int h = 0; h < kv_head_count; h++) {
      matvec(head_dim, embedding_dim,
             &v_cache[MINDEX4(layer_count, kv_head_count, context_len, head_dim,
                              l, h, pos, 0)],
             &mha_norm[MINDEX0()],
             &mha_v_weight[MINDEX4(layer_count, kv_head_count, head_dim,
                                   embedding_dim, l, h, 0, 0)]);
    }
  }
  __ghost([&]() {
    __consumes("&embedding[MINDEX0()] ~> Matrix1(embedding_dim)");
    __consumes("&mha_norm[MINDEX0()] ~> Matrix1(embedding_dim)");
    __produces("embedding ~> Matrix1(embedding_dim)");
    __produces("mha_norm ~> UninitMatrix1(embedding_dim)");
    __admitted();
  });
  free(embedding);
  free(mha_norm);
  free(mha_q);
}
