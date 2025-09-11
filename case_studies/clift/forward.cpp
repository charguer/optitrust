
#include "dumper.h"
#include <math.h>
#include <optitrust.h>

void rmsnorm(int col_count, float *y, float *x, float *w, float epsilon) {

  // calculate sum of squares
  float ss = 0.0f;
  for (int j = 0; j < col_count; j++) {
    ss +=
        x[MINDEX1(col_count, j)] *
        x[MINDEX1(col_count,
                  j)];
  }
  ss /= col_count;
  ss += epsilon;
  ss = 1.0f / sqrtf(ss);

  // normalize and scale
  for (int j = 0; j < col_count; j++) {
    y[MINDEX1(col_count, j)] =
        w[MINDEX1(col_count, j)] *
        (ss * x[MINDEX1(col_count, j)]);
  }
}

void softmax(int col_count, int col_stride, float *x) {

  // find max value (for numerical stability)
  float max_val = x[MINDEX1(col_count, 0)];
  for (int j = 1; j < col_count; j++) {
    if (x[MINDEX1(col_count, j)] > max_val) {
      max_val =
          x[MINDEX1(col_count, j)];
    }
  }
  // exp and sum
  float sum = 0.0f;
  for (int j = 0; j < col_count; j++) {
    x[MINDEX1(col_count, j)] = expf(x[MINDEX1(col_count, j)] - max_val);
    sum += x[MINDEX1(col_count, j)];
  }
  // normalize
  for (int j = 0; j < col_count; j++) {
    x[MINDEX1(col_count, j)] /= sum;
  }
}

void matvec(int col_count, int red_count, float *y, float *x, float *w) {
  for (int j = 0; j < col_count; j++) {
    y[MINDEX1(col_count, j)] = 0.0f;
    for (int k = 0; k < red_count; k++) {
      y[MINDEX1(col_count, j)] +=
          x[MINDEX1(red_count, k)] *
          w[MINDEX2(col_count, red_count, j, k)];

    }
  }
}

void rope(int col_count, float *x, int pos) {

  for (int j = 0; j < col_count; j += 2) {
    float freq = 1.0f / powf(500000.0f, j / (float)col_count);
    float val = (pos)*freq;
    float fcr = cosf(val);
    float fci = sinf(val);
    float v0 = x[MINDEX1(col_count, j)];
    float v1 = x[MINDEX1(col_count, j + 1)];
    x[MINDEX1(col_count, j)] = v0 * fcr - v1 * fci;
    x[MINDEX1(col_count, j + 1)] = v0 * fci + v1 * fcr;
  }
}
void matmul(int row_count, int col_count, int red_count, float *const x,
            float *const y, float *const w) {

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
             int hidden_dim,

             float epsilon,

             float *embedding_weight, float *mha_norm_weight,
             float *mha_q_weight, float *mha_k_weight, float *mha_v_weight,
             float *mha_out_weight, float *ffn_norm_weight,
             float *ffn_fc_weight, float *ffn_up_weight, float *ffn_out_weight,
             float *out_norm_weight, float *out_weight,

             float *k_cache, float *v_cache, float *logits, int pos,
             int logits_count) {

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
  for (int e = 0; e < embedding_dim; e++) {
    embedding[MINDEX1(embedding_dim, e)] =
        embedding_weight[MINDEX2(vocabulary_len, embedding_dim, token, e)];
  }

  // forward all the layers
  for (int l = 0; l < layer_count; l++) {

    // attention rmsnorm
    rmsnorm(embedding_dim, &mha_norm[MINDEX0()], &embedding[MINDEX0()],
            &(mha_norm_weight[MINDEX2(layer_count, embedding_dim, l, 0)]),
            epsilon);
    // qkv matvecs for this position
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
    // multihead attention. iterate over all heads
    for (int q = 0; q < q_head_count; q++) {
      int h = q / q_head_per_kv_head_count;
      for (int p = 0; p <= pos; p++) {
        mha_score[MINDEX2(q_head_count, context_len, q, p)] = 0.0f;
        for (int e = 0; e < head_dim; e++) {
          mha_score[MINDEX2(q_head_count, context_len, q, p)] +=
              mha_q[MINDEX2(q_head_count, head_dim, q, e)] *
              k_cache[MINDEX4(layer_count, kv_head_count, context_len, head_dim,
                              l, h, p,
                              e)]; // donner un nom a ça
        }
        mha_score[MINDEX2(q_head_count, context_len, q, p)] /= sqrtf(head_dim);
      }

      // softmax the scores to get attention weights
      softmax(pos + 1, context_len,
              &mha_score[MINDEX2(q_head_count, context_len, q, 0)]);

      // weighted sum of the values
      for (int e = 0; e < head_dim; e++) {
        mha_blend[MINDEX2(q_head_count, head_dim, q, e)] = 0.0f;
      }

      for (int p = 0; p <= pos; p++) {
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
    // final matvec to get the output of the attention
    matvec(embedding_dim, embedding_dim, &mha_out[MINDEX0()],
           &mha_att[MINDEX0()],
           &mha_out_weight[MINDEX3(layer_count, embedding_dim, embedding_dim, l,
                                   0, 0)]);

    // residual connection back into x

    for (int e = 0; e < embedding_dim; e++) {
      embedding[MINDEX1(embedding_dim, e)] +=
          mha_out[MINDEX1(embedding_dim, e)];
    }
    // ffn rmsnorm
    rmsnorm(embedding_dim, &ffn_norm[MINDEX0()], &embedding[MINDEX0()],
            &ffn_norm_weight[MINDEX2(layer_count, embedding_dim, l, 0)],
            epsilon);
    matvec(hidden_dim, embedding_dim, &ffn_fc[MINDEX0()], &ffn_norm[MINDEX0()],
           &ffn_fc_weight[MINDEX3(layer_count, hidden_dim, embedding_dim, l, 0,
                                  0)]);
    matvec(hidden_dim, embedding_dim, &ffn_up[MINDEX0()], &ffn_norm[MINDEX0()],
           &ffn_up_weight[MINDEX3(layer_count, hidden_dim, embedding_dim, l, 0,
                                  0)]);
    // SwiGLU non-linearity
    for (int e = 0; e < hidden_dim; e++) {
      ffn_fc[MINDEX1(hidden_dim, e)] *=
          (1.0f / (1.0f + expf(-ffn_fc[MINDEX1(hidden_dim, e)])));
      ffn_fc[MINDEX1(hidden_dim, e)] *= ffn_up[MINDEX1(hidden_dim, e)];
    }
    // final matvec to get the output of the ffn
    matvec(embedding_dim, hidden_dim, &ffn_out[MINDEX0()], &ffn_fc[MINDEX0()],
           &ffn_out_weight[MINDEX3(layer_count, embedding_dim, hidden_dim, l, 0,
                                   0)]);
    // residual connection
    for (int e = 0; e < embedding_dim; e++) {
      embedding[MINDEX1(embedding_dim, e)] +=
          ffn_out[MINDEX1(embedding_dim, e)];
    }
  }
  // final rmsnorm#include <optitrust.h>
  rmsnorm(embedding_dim, &embedding[MINDEX0()], &embedding[MINDEX0()],
          out_norm_weight, epsilon);
  // classifier into logits
  if(logits_count) {
  matvec(vocabulary_len, embedding_dim, &logits[MINDEX0()],
         &embedding[MINDEX0()], out_weight);
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

    forward(sequence[i], vocabulary_len, context_len, layer_count, q_head_count,
            kv_head_count, q_head_per_kv_head_count, embedding_dim, head_dim,
            q_dim, kv_dim, hidden_dim, epsilon, embedding_weight,
            mha_norm_weight, mha_q_weight, mha_k_weight, mha_v_weight,
            mha_out_weight, ffn_norm_weight, ffn_fc_weight, ffn_up_weight,
            ffn_out_weight, out_norm_weight, out_weight, k_cache, v_cache,
            logits, i, (i == (sequence_len - 1) ? 1 : 0));
  }
}
