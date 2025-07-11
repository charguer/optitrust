#include <math.h>
#include <optitrust.h>

#include "dumper.h"
void iter_matvec(int row_count, int col_count, int red_count, float *const x,
                 float *const y, float *const w) {
  for (int i = 0; i < row_count; i++) {
    matvec(col_count, red_count, &x[MINDEX2(row_count, col_count, i, 0)],
           &y[MINDEX2(row_count, red_count, i, 0)], w);
  }
}
void rmsnorm(int col_count, float *y, float *x, float *w, float epsilon) {
  float ss = 0.f;
  for (int j = 0; j < col_count; j++) {
    ss += x[MINDEX1(col_count, j)] * x[MINDEX1(col_count, j)];
  }
  ss /= col_count;
  ss += epsilon;
  ss = 1.f / sqrtf(ss);
  for (int j = 0; j < col_count; j++) {
    y[MINDEX1(col_count, j)] =
        w[MINDEX1(col_count, j)] * (ss * x[MINDEX1(col_count, j)]);
  }
}

void softmax(int col_count, int col_stride, float *x) {
  float max_val = x[MINDEX1(col_count, 0)];
  for (int j = 1; j < col_count; j++) {
    if (x[MINDEX1(col_count, j)] > max_val) {
      max_val = x[MINDEX1(col_count, j)];
    }
  }
  float sum = 0.f;
  for (int j = 0; j < col_count; j++) {
    x[MINDEX1(col_count, j)] = expf(x[MINDEX1(col_count, j)] - max_val);
    sum += x[MINDEX1(col_count, j)];
  }
  for (int j = 0; j < col_count; j++) {
    x[MINDEX1(col_count, j)] /= sum;
  }
}
void matmul(int row_count, int col_count, int red_count, float *y, float *x,
            float *w) {

  for (int i = 0; i < row_count; i++) {
    for (int j = 0; j < col_count; j++) {
      y[MINDEX2(row_count, col_count, i, j)] = 0.f;
      for (int k = 0; k < red_count; k++) {
        y[MINDEX2(row_count, col_count, i, j)] +=
            x[MINDEX2(row_count, red_count, i, k)] *
            w[MINDEX2(col_count, red_count, j, k)];
      }
    }
  }
}

void matvec(int col_count, int red_count, float *y, float *x, float *w) {
  for (int j = 0; j < col_count; j++) {
    y[MINDEX1(col_count, j)] = 0.f;
    for (int k = 0; k < red_count; k++) {
      y[MINDEX1(col_count, j)] +=
          x[MINDEX1(red_count, k)] * w[MINDEX2(col_count, red_count, j, k)];
    }
  }
}

void rope(int col_count, float *x, int pos) {
  for (int j = 0; j < col_count; j += 2) {
    float freq = 1.f / powf(500000.f, j / (float)col_count);
    float val = pos * freq;
    float fcr = cosf(val);
    float fci = sinf(val);
    float v0 = x[MINDEX1(col_count, j)];
    float v1 = x[MINDEX1(col_count, j + 1)];
    x[MINDEX1(col_count, j)] = v0 * fcr - v1 * fci;
    x[MINDEX1(col_count, j + 1)] = v0 * fci + v1 * fcr;
  }
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
  float *const embedding =
      (float *)malloc(MSIZE2(sequence_len, embedding_dim) * sizeof(float));
  float *const mha_norm =
      (float *)malloc(MSIZE2(sequence_len, embedding_dim) * sizeof(float));
  float *const mha_q = (float *)malloc(
      MSIZE3(sequence_len, q_head_count, head_dim) * sizeof(float));
  float *const mha_score = (float *)malloc(
      MSIZE3(sequence_len, q_head_count, context_len) * sizeof(float));
  float *const mha_blend = (float *)malloc(
      MSIZE3(sequence_len, q_head_count, head_dim) * sizeof(float));
  float *const mha_att =
      (float *)malloc(MSIZE2(sequence_len, embedding_dim) * sizeof(float));
  float *const mha_out =
      (float *)malloc(MSIZE2(sequence_len, embedding_dim) * sizeof(float));
  float *const ffn_norm =
      (float *)malloc(MSIZE2(sequence_len, embedding_dim) * sizeof(float));
  float *const ffn_fc =
      (float *)malloc(MSIZE2(sequence_len, hidden_dim) * sizeof(float));
  float *const ffn_up =
      (float *)malloc(MSIZE2(sequence_len, hidden_dim) * sizeof(float));
  float *const ffn_out =
      (float *)malloc(MSIZE2(sequence_len, embedding_dim) * sizeof(float));
  for (int i = 0; i < sequence_len; i++) {
    for (int e = 0; e < embedding_dim; e++) {
      embedding[MINDEX2(sequence_len, embedding_dim, i, e)] =
          embedding_weight[MINDEX2(vocabulary_len, embedding_dim, sequence[i],
                                   e)];
    }
  }
  for (int l = 0; l < layer_count; l++) {
    for (int i = 0; i < sequence_len; i++) {
      rmsnorm(
          embedding_dim, &mha_norm[MINDEX2(sequence_len, embedding_dim, i, 0)],
          &embedding[MINDEX2(sequence_len, embedding_dim, i, 0)],
          &mha_norm_weight[MINDEX2(layer_count, embedding_dim, l, 0)], epsilon);
    }
    for (int q = 0; q < q_head_count; q++) {
      for (int i = 0; i < sequence_len; i++) {
        matvec(head_dim, embedding_dim,
               &mha_q[MINDEX3(q_head_count, sequence_len, head_dim, q, i, 0)],
               &mha_norm[MINDEX2(sequence_len, embedding_dim, i, 0)],
               &mha_q_weight[MINDEX4(layer_count, q_head_count, head_dim,
                                     embedding_dim, l, q, 0, 0)]);
      }
    }
    for (int h = 0; h < kv_head_count; h++) {
      for (int i = 0; i < sequence_len; i++) {
        matvec(head_dim, embedding_dim,
               &k_cache[MINDEX4(layer_count, kv_head_count, context_len,
                                head_dim, l, h, i, 0)],
               &mha_norm[MINDEX2(sequence_len, embedding_dim, i, 0)],
               &mha_k_weight[MINDEX4(layer_count, kv_head_count, head_dim,
                                     embedding_dim, l, h, 0, 0)]);
      }
    }
    for (int h = 0; h < kv_head_count; h++) {
      for (int i = 0; i < sequence_len; i++) {
        matvec(head_dim, embedding_dim,
               &v_cache[MINDEX4(layer_count, kv_head_count, context_len,
                                head_dim, l, h, i, 0)],
               &mha_norm[MINDEX2(sequence_len, embedding_dim, i, 0)],
               &mha_v_weight[MINDEX4(layer_count, kv_head_count, head_dim,
                                     embedding_dim, l, h, 0, 0)]);
      }
    }
    for (int q = 0; q < q_head_count; q++) {
      for (int i = 0; i < sequence_len; i++) {
        rope(head_dim,
             &mha_q[MINDEX3(q_head_count, sequence_len, head_dim, q, i, 0)], i);
      }
    }
    for (int h = 0; h < kv_head_count; h++) {
      for (int i = 0; i < sequence_len; i++) {
        rope(head_dim,
             &k_cache[MINDEX4(layer_count, kv_head_count, context_len, head_dim,
                              l, h, i, 0)],
             i);
      }
    }
    for (int q = 0; q < q_head_count; q++) {
      for (int i = 0; i < sequence_len; i++) {
        int h = q / q_head_per_kv_head_count;
        for (int p = 0; p <= i; p++) {
          mha_score[MINDEX3(q_head_count, sequence_len, context_len, q, i, p)] =
              0.f;
          for (int e = 0; e < head_dim; e++) {
            mha_score[MINDEX3(q_head_count, sequence_len, context_len, q, i,
                              p)] +=
                mha_q[MINDEX3(q_head_count, sequence_len, head_dim, q, i, e)] *
                k_cache[MINDEX4(layer_count, kv_head_count, context_len,
                                head_dim, l, h, p, e)];
          }
          mha_score[MINDEX3(q_head_count, sequence_len, context_len, q, i,
                            p)] /= sqrtf(head_dim);
        }
        softmax(i + 1, context_len,
                &mha_score[MINDEX3(q_head_count, sequence_len, context_len, q,
                                   i, 0)]);
        for (int e = 0; e < head_dim; e++) {
          mha_blend[MINDEX3(q_head_count, sequence_len, head_dim, q, i, e)] =
              0.f;
        }
        for (int p = 0; p <= i; p++) {
          for (int e = 0; e < head_dim; e++) {
            mha_blend[MINDEX3(q_head_count, sequence_len, head_dim, q, i, e)] +=
                mha_score[MINDEX3(q_head_count, sequence_len, context_len, q, i,
                                  p)] *
                v_cache[MINDEX4(layer_count, kv_head_count, context_len,
                                head_dim, l, h, p, e)];
          }
        }
      }
    }
    for (int q = 0; q < q_head_count; q++) {
      for (int i = 0; i < sequence_len; i++) {
        for (int e = 0; e < head_dim; e++) {
          mha_att[MINDEX2(sequence_len, embedding_dim, i, q * head_dim + e)] =
              mha_blend[MINDEX3(q_head_count, sequence_len, head_dim, q, i, e)];
        }
      }
    }
    for (int i = 0; i < sequence_len; i++) {
      matvec(embedding_dim, embedding_dim,
             &mha_out[MINDEX2(sequence_len, embedding_dim, i, 0)],
             &mha_att[MINDEX2(sequence_len, embedding_dim, i, 0)],
             &mha_out_weight[MINDEX3(layer_count, embedding_dim, embedding_dim,
                                     l, 0, 0)]);
    }
    for (int i = 0; i < sequence_len; i++) {
      for (int e = 0; e < embedding_dim; e++) {
        embedding[MINDEX2(sequence_len, embedding_dim, i, e)] +=
            mha_out[MINDEX2(sequence_len, embedding_dim, i, e)];
      }
    }
    for (int i = 0; i < sequence_len; i++) {
      rmsnorm(
          embedding_dim, &ffn_norm[MINDEX2(sequence_len, embedding_dim, i, 0)],
          &embedding[MINDEX2(sequence_len, embedding_dim, i, 0)],
          &ffn_norm_weight[MINDEX2(layer_count, embedding_dim, l, 0)], epsilon);
    }
    for (int i = 0; i < sequence_len; i++) {
      matvec(hidden_dim, embedding_dim,
             &ffn_fc[MINDEX2(sequence_len, hidden_dim, i, 0)],
             &ffn_norm[MINDEX2(sequence_len, embedding_dim, i, 0)],
             &ffn_fc_weight[MINDEX3(layer_count, hidden_dim, embedding_dim, l,
                                    0, 0)]);
    }
    for (int i = 0; i < sequence_len; i++) {
      matvec(hidden_dim, embedding_dim,
             &ffn_up[MINDEX2(sequence_len, hidden_dim, i, 0)],
             &ffn_norm[MINDEX2(sequence_len, embedding_dim, i, 0)],
             &ffn_up_weight[MINDEX3(layer_count, hidden_dim, embedding_dim, l,
                                    0, 0)]);
    }
    for (int i = 0; i < sequence_len; i++) {
      for (int e = 0; e < hidden_dim; e++) {
        ffn_fc[MINDEX2(sequence_len, hidden_dim, i, e)] *=
            1.f /
            (1.f + expf(-ffn_fc[MINDEX2(sequence_len, hidden_dim, i, e)]));
        ffn_fc[MINDEX2(sequence_len, hidden_dim, i, e)] *=
            ffn_up[MINDEX2(sequence_len, hidden_dim, i, e)];
      }
    }
    for (int i = 0; i < sequence_len; i++) {
      matvec(embedding_dim, hidden_dim,
             &ffn_out[MINDEX2(sequence_len, embedding_dim, i, 0)],
             &ffn_fc[MINDEX2(sequence_len, hidden_dim, i, 0)],
             &ffn_out_weight[MINDEX3(layer_count, embedding_dim, hidden_dim, l,
                                     0, 0)]);
    }
    for (int i = 0; i < sequence_len; i++) {
      for (int e = 0; e < embedding_dim; e++) {
        embedding[MINDEX2(sequence_len, embedding_dim, i, e)] +=
            ffn_out[MINDEX2(sequence_len, embedding_dim, i, e)];
      }
    }
  }
  for (int i = 0; i < sequence_len; i++) {
    rmsnorm(embedding_dim,
            &embedding[MINDEX2(sequence_len, embedding_dim, i, 0)],
            &embedding[MINDEX2(sequence_len, embedding_dim, i, 0)],
            out_norm_weight, epsilon);
  }
  for (int i = 0; i < sequence_len; i++) {
    matvec(vocabulary_len, embedding_dim, &logits[MINDEX0()],
           &embedding[MINDEX2(sequence_len, embedding_dim, i, 0)], out_weight);
  }
  free(ffn_out);
  free(ffn_up);
  free(ffn_fc);
  free(ffn_norm);
  free(mha_out);
  free(mha_att);
  free(mha_blend);
  free(mha_score);
  free(mha_q);
  free(mha_norm);
  free(embedding);
}
