#ifndef __APAC_PROFILER_HPP
#define __APAC_PROFILER_HPP

#include <iostream>
#include <fstream>
#include <string>
#include <chrono>
#include <fstream>
#include <filesystem>
#include <type_traits>
#include <vector>

bool isFirstEvent = true;

/// This namespace include the utils function to perform meta programming
namespace ApacMetaUtils{

// Check if a type is a vector
template<class T> struct is_stdvector : public std::false_type {};

template<class T, class Alloc>
struct is_stdvector<std::vector<T, Alloc>> : public std::true_type {
    using _T = T;
};

template<class T, class Alloc>
struct is_stdvector<const std::vector<T, Alloc>> : public std::true_type {
    using _T = T;
};

// Check if an object has size method
template <typename, typename = std::void_t<>>
struct has_size_methode
    : public std::false_type {};

template <typename Class>
struct has_size_methode<Class,
                        std::void_t<decltype(std::declval<Class>().size())>>
    : public std::is_arithmetic<decltype(std::declval<Class>().size())>
{};


// Check if an object has getSize method
template <typename, typename = std::void_t<>>
struct has_getsize_methode
    : public std::false_type {};

template <typename Class>
struct has_getsize_methode<Class,
                           std::void_t<decltype(std::declval<Class>().getSize())>>
    : public std::is_arithmetic<decltype(std::declval<Class>().getSize())>
{};

// Check if an object has nbElements method
template <typename, typename = std::void_t<>>
struct has_nbelements_methode
    : public std::false_type {};

template <typename Class>
struct has_nbelements_methode<Class,
                              std::void_t<decltype(std::declval<Class>().nbElements())>>
    : public std::is_arithmetic<decltype(std::declval<Class>().nbElements())>
{};

}

// Simple timer to record execution time (on CPU)
// This is not the CPU time but the elapsed time
class ApacProfileTimer {
    using double_second_time = std::chrono::duration<double, std::ratio<1, 1>>;
    std::chrono::high_resolution_clock::time_point m_start;
    std::chrono::high_resolution_clock::time_point m_end;
    std::chrono::nanoseconds m_cumulate;
public:
    ApacProfileTimer() : m_cumulate(0) { start(); }
    template <class DepType>
    explicit ApacProfileTimer(DepType& inStuff)  : m_cumulate(0) { start(); }
    ApacProfileTimer(const ApacProfileTimer &other) = delete;
    ApacProfileTimer &operator=(const ApacProfileTimer &other) = delete;
    ApacProfileTimer(ApacProfileTimer &&other) = default;
    ApacProfileTimer &operator=(ApacProfileTimer &&other) = default;
    void reset() {
        m_start = std::chrono::high_resolution_clock::time_point();
        m_end = std::chrono::high_resolution_clock::time_point();
        m_cumulate = std::chrono::nanoseconds();
        start();
    }
    void start() { m_start = std::chrono::high_resolution_clock::now(); }
    template <class DepType>
    void start(DepType& inStuff) { start(); }
    void stop() {
        m_end = std::chrono::high_resolution_clock::now();
        m_cumulate +=
                std::chrono::duration_cast<std::chrono::nanoseconds>(m_end - m_start);
    }
    template <class DepType>
    void stop(DepType& inStuff) { stop(); }
    double getElapsed() const {
        return std::chrono::duration_cast<std::chrono::nanoseconds>(m_end -
                                                                         m_start)
                .count()/1e9;
    }
    double getCumulated() const {
        return m_cumulate.count()/1e9;
    }
    double stopAndGetElapsed() {
        stop();
        return getElapsed()/1e9;
    }
};

class ApacProfilerSection {
  std::string currentLine;
  ApacProfileTimer profileTimer;
  const std::string profileFile;
  
public:
  ApacProfilerSection(const std::string inTaskName,
                      const int inNbParams) : profileFile("apac_profile") {
    currentLine.append("{ ");
    currentLine.append(inTaskName);
    currentLine.append(", ");
    currentLine.append(std::to_string(inNbParams));
    currentLine.append(", ");
  }
  
  // This method should be called for each param that the task will depend on
  template <class T>
  void addParam(const char inMode, const T & inParam) {
    currentLine.append(1, inMode);
    currentLine.append("=");
    
    using RawType = typename std::decay<T>::type;
    if constexpr(ApacMetaUtils::is_stdvector<RawType>::value){
      currentLine.append(std::to_string(inParam.size()));
    }
    else if constexpr (ApacMetaUtils::has_size_methode<RawType>::value){
      currentLine.append(std::to_string(inParam.size()));
    }
    else if constexpr (ApacMetaUtils::has_getsize_methode<RawType>::value){
      currentLine.append(std::to_string(inParam.getSize()));
    }
    else if constexpr (ApacMetaUtils::has_nbelements_methode<RawType>::value){
      currentLine.append(std::to_string(inParam.nbElements()));
    }
    else if constexpr (std::is_arithmetic<RawType>::value){
      currentLine.append(std::to_string(inParam));
    }
    else{
      currentLine.append(std::to_string(sizeof(RawType)));
    }
    
    currentLine.append(", ");
  }
  
  // This should be called right before calling the target function
  void beforeCall(){
    profileTimer.start();
  }
  
  // This should be called right after the target function
  void afterCall(){
    profileTimer.stop();

    if (isFirstEvent) {
      if (std::filesystem::exists(profileFile)) {
        std::remove(profileFile.c_str());
      }

      std::ofstream profileStream(profileFile, std::ios_base::app);
      profileStream.close();
      isFirstEvent = false;
    }
    
    std::ofstream profileStream(profileFile, std::ios_base::app);
    if(!profileStream.is_open()){
      throw std::runtime_error("Error opening profile file!");
    }
    
    currentLine.append(std::to_string(profileTimer.getElapsed()));
    currentLine.append(" }\n");
    profileStream << currentLine;
    currentLine.clear();
  }
};

#endif // __APAC_PROFILER_HPP
