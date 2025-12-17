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

void rope(int col_count, float *x, int pos) {
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

void softmax(int col_count, int col_stride, float *x) {
  __modifies("x ~>Matrix1(col_count)");
  // find max value (for numerical stability)
  float max_val = x[MINDEX1(col_count, 0)];
  __GHOST_BEGIN(focus_subrange, group_focus_subrange,
                "sub_range := 1..col_count");
  for (int j = 1; j < col_count; j++) {
    __xmodifies("&x[MINDEX1(col_count, j)] ~> Cell");
    if (x[MINDEX1(col_count, j)] > max_val) {
      max_val = x[MINDEX1(col_count, j)];
    }
  }
  __GHOST_END(focus_subrange);
  // exp and sum
  float sum = 0.0f;
  for (int j = 0; j < col_count; j++) {
    x[MINDEX1(col_count, j)] = _expf(x[MINDEX1(col_count, j)] - max_val);
    sum += x[MINDEX1(col_count, j)];
  }
  // normalize
  for (int j = 0; j < col_count; j++) {
    x[MINDEX1(col_count, j)] /= sum;
  }
}
void rmsnorm(int col_count, float *y, float *x, float *w, float epsilon) {
  __modifies("y ~> Matrix1(col_count)");
  __reads("x ~> Matrix1(col_count)");
  __reads("w ~> Matrix1(col_count)");
  float ss = 0.f;
  for (int j = 0; j < col_count; j++) {
    const float x_j = x[MINDEX1(col_count, j)];
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

void matvec(int col_count, int red_count, float *x, float *y, float *w) {
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
void matmul(int col_count, int red_count, int row_count, float *x, float *y,
            float *w) {
  __modifies("x ~> Matrix2(row_count,col_count)");
  __reads(
      "y ~> Matrix2(row_count,red_count),w ~> Matrix2(col_count, red_count)");
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
  __reads("embedding_weight ~> Matrix2(vocabulary_len, embedding_dim)");
  __reads("mha_norm_weight ~> Matrix2(layer_count, embedding_dim)");
  __reads("mha_q_weight ~> Matrix4(layer_count, q_head_count, head_dim, "
          "embedding_dim)");
  __reads("mha_k_weight ~> Matrix4(layer_count, kv_head_count, head_dim, "
          "embedding_dim)");
  __reads("mha_v_weight ~> Matrix4(layer_count, kv_head_count, head_dim, "
          "embedding_dim)");
  __modifies(
      "k_cache ~> Matrix4(layer_count,kv_head_count, context_len, head_dim)");
  __modifies(
      "v_cache ~> Matrix4(layer_count,kv_head_count, context_len, head_dim)");
  __reads("mha_out_weight ~> Matrix3(layer_count,embedding_dim,embedding_dim)");
  __reads("ffn_norm_weight ~> Matrix2(layer_count, embedding_dim)");
  __reads("ffn_fc_weight ~> Matrix3(layer_count,hidden_dim,embedding_dim)");
  __reads("ffn_up_weight ~> Matrix3(layer_count,hidden_dim,embedding_dim)");
  __reads("ffn_out_weight ~> Matrix3(layer_count,embedding_dim,hidden_dim)");
  __reads("out_norm_weight ~> Matrix1(embedding_dim)");
  __reads("out_weight ~> Matrix2(vocabulary_len,embedding_dim)");
  __modifies("logits ~> Matrix1(vocabulary_len)");

  float *const embedding = CALLOC1(float, embedding_dim);
  float *const embedding_final = CALLOC1(float, embedding_dim);
  float *const mha_norm = CALLOC1(float, embedding_dim);
  float *const mha_q = CALLOC2(float, q_head_count, head_dim);
  float *const mha_score = CALLOC2(float, q_head_count, context_len);
  float *const mha_blend = CALLOC2(float, q_head_count, head_dim);
  float *const mha_att = CALLOC1(float, embedding_dim);
  float *const mha_out = CALLOC1(float, embedding_dim);
  float *const ffn_norm = CALLOC1(float, embedding_dim);
  float *const ffn_fc = CALLOC1(float, hidden_dim);
  float *const ffn_up = CALLOC1(float, hidden_dim);
  float *const ffn_out = CALLOC1(float, embedding_dim);

  for (int e = 0; e < embedding_dim; e++) {
    embedding[MINDEX1(embedding_dim, e)] =
        embedding_weight[MINDEX2(vocabulary_len, embedding_dim, token, e)];
  }
  __ghost([&]() {
    // __consumes("embedding ~> Matrix1(embedding_dim)");
    // __consumes("mha_out ~> Matrix1(embedding_dim)");
    __consumes("mha_norm ~> UninitMatrix1(embedding_dim)");
    // __produces("&embedding[MINDEX0()] ~> Matrix1(embedding_dim)");
    __produces("&mha_norm[MINDEX0()] ~> Matrix1(embedding_dim)");
    // __produces("&mha_out[MINDEX0()] ~> Matrix1(embedding_dim)");
    __admitted();
  });
  for (int l = 0; l < layer_count; l++) {
    rmsnorm(embedding_dim, &mha_norm[MINDEX0()], embedding,
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
    // RoPE q: complex-valued rotate q in each head
    for (int q = 0; q < q_head_count; q++) {
      rope(head_dim, &mha_q[MINDEX2(q_head_count, head_dim, q, 0)], pos);
    }
    // RoPE k: complex-valued rotate k in each head
    for (int h = 0; h < kv_head_count; h++) {
      rope(head_dim,
           &k_cache[MINDEX4(layer_count, kv_head_count, context_len, head_dim,
                            l, h, pos, 0)],
           pos);
    }
    for (int q = 0; q < q_head_count; q++) {
      const int h = q / q_head_per_kv_head_count;
      for (int p = 0; p < pos + 1; p++) {
        mha_score[MINDEX2(q_head_count, context_len, q, p)] = 0.0f;
        for (int e = 0; e < head_dim; e++) {
          mha_score[MINDEX2(q_head_count, context_len, q, p)] +=
              mha_q[MINDEX2(q_head_count, head_dim, q, e)] *
              k_cache[MINDEX4(layer_count, kv_head_count, context_len, head_dim,
                              l, h, p,
                              e)]; // donner un nom a ça
        }
        mha_score[MINDEX2(q_head_count, context_len, q, p)] /=
            _sqrtf((float)head_dim);
      }
      // softmax the scores to get attention weights
      softmax(context_len, context_len,
              &mha_score[MINDEX2(q_head_count, context_len, q, 0)]);

      // weighted sum of the values
      for (int e = 0; e < head_dim; e++) {
        mha_blend[MINDEX2(q_head_count, head_dim, q, e)] = 0.0f;
      }

      for (int p = 0; p < pos + 1; p++) {
        for (int e = 0; e < head_dim; e++) {
          mha_blend[MINDEX2(q_head_count, head_dim, q, e)] +=
              mha_score[MINDEX2(q_head_count, context_len, q, p)] *
              v_cache[MINDEX4(layer_count, kv_head_count, context_len, head_dim,
                              l, h, p, e)];
        }
      }
    }
    for (int q = 0; q < q_head_count; q++) {
      for (int e = 0; e < head_dim; e++) {
        mha_att[MINDEX1(embedding_dim, q * head_dim + e)] =
            mha_blend[MINDEX2(q_head_count, head_dim, q, e)];
      }
    }
    matvec(embedding_dim, embedding_dim, mha_out, mha_att,
           &mha_out_weight[MINDEX3(layer_count, embedding_dim, embedding_dim, l,
                                   0, 0)]);
    // residual connection back into x
    for (int e = 0; e < embedding_dim; e++) {
      embedding[MINDEX1(embedding_dim, e)] +=
          mha_out[MINDEX1(embedding_dim, e)];
    }
    rmsnorm(embedding_dim, ffn_norm, embedding,
            &ffn_norm_weight[MINDEX2(layer_count, embedding_dim, l, 0)],
            epsilon);
    matvec(hidden_dim, embedding_dim, ffn_fc, ffn_norm,
           &ffn_fc_weight[MINDEX3(layer_count, hidden_dim, embedding_dim, l, 0,
                                  0)]);
    matvec(hidden_dim, embedding_dim, ffn_up, ffn_norm,
           &ffn_up_weight[MINDEX3(layer_count, hidden_dim, embedding_dim, l, 0,
                                  0)]);
    // SwiGLU non-linearity
    for (int e = 0; e < hidden_dim; e++) {
      ffn_fc[MINDEX1(hidden_dim, e)] *=
          (1.0f / (1.0f + _expf(-ffn_fc[MINDEX1(hidden_dim, e)])));
      ffn_fc[MINDEX1(hidden_dim, e)] *= ffn_up[MINDEX1(hidden_dim, e)];
    }
    // final matvec to get the output of the ffn
    matvec(embedding_dim, hidden_dim, ffn_out, ffn_fc,
           &ffn_out_weight[MINDEX3(layer_count, embedding_dim, hidden_dim, l, 0,
                                   0)]);
    // residual connection
    for (int e = 0; e < embedding_dim; e++) {
      embedding[MINDEX1(embedding_dim, e)] +=
          ffn_out[MINDEX1(embedding_dim, e)];
    }
  }
  rmsnorm(embedding_dim, embedding_final, embedding, out_norm_weight, epsilon);
  // classifier into logits
  if (logits_count) {
    matvec(vocabulary_len, embedding_dim, logits, embedding_final, out_weight);
  }

  __ghost([&]() {
    // __consumes("&embedding[MINDEX0()] ~> Matrix1(embedding_dim)");
    __consumes("&mha_norm[MINDEX0()] ~> Matrix1(embedding_dim)");
    // __produces("embedding ~> Matrix1(embedding_dim)");
    __produces("mha_norm ~> UninitMatrix1(embedding_dim)");
    __admitted();
  });
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
  free(embedding_final);
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
  __reads("embedding_weight ~> Matrix2(vocabulary_len, embedding_dim)");
  __reads("mha_norm_weight ~> Matrix2(layer_count, embedding_dim)");
  __reads("mha_q_weight ~> Matrix4(layer_count, q_head_count, head_dim, "
          "embedding_dim)");
  __reads("mha_k_weight ~> Matrix4(layer_count, kv_head_count, head_dim, "
          "embedding_dim)");
  __reads("mha_v_weight ~> Matrix4(layer_count, kv_head_count, head_dim, "
          "embedding_dim)");
  __modifies(
      "k_cache ~> Matrix4(layer_count,kv_head_count, context_len, head_dim)");
  __modifies(
      "v_cache ~> Matrix4(layer_count,kv_head_count, context_len, head_dim)");
  __reads("mha_out_weight ~> Matrix3(layer_count,embedding_dim,embedding_dim)");
  __reads("ffn_norm_weight ~> Matrix2(layer_count, embedding_dim)");
  __reads("ffn_fc_weight ~> Matrix3(layer_count,hidden_dim,embedding_dim)");
  __reads("ffn_up_weight ~> Matrix3(layer_count,hidden_dim,embedding_dim)");
  __reads("ffn_out_weight ~> Matrix3(layer_count,embedding_dim,hidden_dim)");
  __reads("out_norm_weight ~> Matrix1(embedding_dim)");
  __reads("out_weight ~> Matrix2(vocabulary_len,embedding_dim)");
  __modifies("logits ~> Matrix1(vocabulary_len)");
  __reads("sequence ~> Matrix1(sequence_len)");
  for (int i = 0; i < sequence_len; i++) {
    __xreads("&sequence[MINDEX1(sequence_len, i)] ~> Cell");
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
