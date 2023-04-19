// [...]


    cv::ocl::setUseOpenCL(false);

    cv::Mat cv_in(h, w, CV_32FC3);
    cv::Mat cv_gray(h, w, CV_32F);
    cv::Mat cv_out(h, w, CV_32F);

    for (int c = 0; c < 3; c++) {
      for (int y = 0; y < input.height(); y++) {
        for (int x = 0; x < input.width(); x++) {
          int i = ((c * input.height() + y) * input.width()) + x;
          cv_in.at<cv::Vec3f>(y, x)[c] = input.data()[i];
        }
      }
    }

    for (int i = 0; i < timing_iterations; i++) {
        auto start = Clock::now();
        cv::cvtColor(cv_in, cv_gray, cv::COLOR_RGB2GRAY);
        cv::cornerHarris(cv_gray, cv_out, 3, 3, 0.04, cv::BORDER_ISOLATED);
        auto stop = Clock::now();

        sample_vec.push_back(std::chrono::duration<double, std::milli>(stop - start).count());
    }

    for (int y = 0; y < output2.height(); y++) {
      for (int x = 0; x < output2.width(); x++) {
        int i = (y * output2.width()) + x;
        output2.data()[i] = cv_out.at<float>(y + 2, x + 2);
      }
    }

// [...]