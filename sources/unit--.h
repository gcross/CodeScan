//@+leo-ver=4-thin
//@+node:gcross.20090108164821.5:@thin unit--.h
//@@language C++
// unit--, a simple and easy-to-use unit test aid for C++
// Copyright (C) 2005~2006  Tsong Chong
// birdiez@126.com
//
//    This program is free software; you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation; either version 2 of the License, or
//    (at your option) any later version.
//
//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.
//
//    You should have received a copy of the GNU General Public License
//    along with this program; if not, write to the Free Software
//    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

#ifndef unitmm_H_opt123
#define unitmm_H_opt123

#include <iostream>
#include <string>
#include <sstream>

//@+others
//@+node:gcross.20090108164821.10:namespace unit_minus
namespace unit_minus {

    class SuiteBase;
    class CaseBase;

    //@    @+others
    //@+node:gcross.20090108164821.7:class Runner
    // log entire test process, providing simple control
    class Runner {
    public:
        virtual ~Runner() {}
        // enter() & exit() are called for every test suite
        virtual void enter(SuiteBase&) =0;
        virtual void exit(SuiteBase&) =0;
        // check() is called for every test case
        virtual void check(CaseBase&) =0;
    };
    //@-node:gcross.20090108164821.7:class Runner
    //@+node:gcross.20090108164821.8:classs TestBase
    //--------------------------------------------------
    // base class of tests
    class TestBase {
        // tests will chain up as a linked list to avoid dependence on
        // dynamic memory allocation init
        virtual TestBase*& nextPointer() = 0;
        friend class SuiteBase;
    public:
        virtual ~TestBase() {}
        virtual void run(Runner&) =0;
        virtual const char* caption() const =0;
    };
    //@nonl
    //@-node:gcross.20090108164821.8:classs TestBase
    //@+node:gcross.20090108164821.11:Suites
    //@+node:gcross.20090108164821.9:class SuiteBase
    //--------------------------------------------------
    struct SuiteState {
        bool added;
        TestBase* tests;
        TestBase* nextPointer;
    };
    // abstract class
    // test suite defination
    class SuiteBase: public TestBase {
    protected:
        SuiteState* m_state;
    private:
        // header of linked list
        TestBase*& tests()
        {
            return m_state->tests;
        }
        virtual TestBase*& nextPointer()
        {
            return m_state->nextPointer;
        }
    public:
        void setState(SuiteState* statep)
        {
            m_state = statep;
        }
        virtual void run(Runner& log)
        {
            log.enter(*this);
            for (TestBase* p = tests(); p != 0; p = p->nextPointer()) {
                p->run(log);
            }
            log.exit(*this);
        }
        bool add(TestBase& test)
        {
            test.nextPointer() = tests();
            tests() = &test;
            return true;
        }
    };
    //@-node:gcross.20090108164821.9:class SuiteBase
    //@+node:gcross.20090108164821.12:function suiteInstance
    // implement singleton work
    template <typename ThisSuite>
    extern inline ThisSuite& suiteInstance()
    {
        static SuiteState state = { false, 0, 0 };
        static ThisSuite s_inst;
        s_inst.setState(&state);
        return s_inst;
    }
    //@nonl
    //@-node:gcross.20090108164821.12:function suiteInstance
    //@+node:gcross.20090108164821.13:class Suite
    // abstract class
    template <class ThisSuite, class UpperSuite>
    class Suite: public SuiteBase {
        typedef Suite Self;
    public:
        bool addToUpper()
        {
            // add will be performed only once
            return m_state->added = m_state->added || suiteInstance<UpperSuite>().add(*this);
        }
    };
    //@nonl
    //@-node:gcross.20090108164821.13:class Suite
    //@+node:gcross.20090108164821.14:class Root (suite)
    // the top most test suite
    // every test case should be attached to Root directly or indirectly
    // it's add to upper is not called, OR there will be a INFINITE LOOP
    class Root: public Suite<Root, Root> {
    public:
        virtual const char* caption() const
        {
            return "all tests";
        }
    };
    //@-node:gcross.20090108164821.14:class Root (suite)
    //@-node:gcross.20090108164821.11:Suites
    //@+node:gcross.20090108164821.15:Test Cases
    //@+node:gcross.20090108164821.16:class CaseBase
    //--------------------------------------------------
    struct CaseState {
        bool added;
        TestBase* nextPointer;
    };
    // test work implementation
    // abstract class
    class CaseBase: public TestBase {
    protected:
        CaseState* m_state;
    private:
        virtual TestBase*& nextPointer()
        {
            return m_state->nextPointer;
        }
    public:
        virtual void test() =0;
        virtual const char* file() const =0;
        virtual int line() const =0;
    public:
        void setState(CaseState* statep)
        {
            m_state = statep;
        }
        virtual void run(Runner& log)
        {
            log.check(*this);
        }
    };
    //@nonl
    //@-node:gcross.20090108164821.16:class CaseBase
    //@+node:gcross.20090108164821.17:function caseInstance
    // singleton work
    template <typename ThisCase>
    extern inline ThisCase& caseInstance()
    {
        static CaseState state = { false, 0 };
        static ThisCase s_inst;
        s_inst.setState(&state);
        return s_inst;
    }
    //@nonl
    //@-node:gcross.20090108164821.17:function caseInstance
    //@+node:gcross.20090108164821.18:class Case
    // abstract class
    // see Suite
    template <class ThisCase, class UpperSuite>
    class Case: public CaseBase {
    private:
        typedef Case Self;
    public:
        bool addToUpper()
        {
            return m_state->added = m_state->added || suiteInstance<UpperSuite>().add(*this);
        }
    };
    //@-node:gcross.20090108164821.18:class Case
    //@-node:gcross.20090108164821.15:Test Cases
    //@+node:gcross.20090108164821.19:Assertions
    //@+node:gcross.20090108164821.20:class AssertionInfo
    //--------------------------------------------------
    class AssertionInfo {
    private:
        bool m_successful;
        bool m_hasMessage;
        std::string m_message;

        std::string m_rawMessage;
    public:
        AssertionInfo(bool successp)
            : m_successful(successp), m_hasMessage(false) {}
        AssertionInfo(bool successp, const std::string& messagep)
            : m_successful(successp), m_hasMessage(true), m_message(messagep) {}
        bool successful() const { return m_successful; }
        operator bool () const { return successful(); }
        bool operator ! () const { return !successful(); }

        bool hasMessage() const { return m_hasMessage; }
        const std::string& message() const { return m_message; }

        const std::string& rawMessage() const { return m_rawMessage; }
        void rawMessage(const std::string& rawMess) { m_rawMessage = rawMess; }
    };
    //@nonl
    //@-node:gcross.20090108164821.20:class AssertionInfo
    //@+node:gcross.20090108164821.21:class AssertionChecker
    class AssertionChecker;
    // singleton, 7.1.2 - 4
    extern inline AssertionChecker*& currentChecker()
    {
        static AssertionChecker* s_ptr;
        return s_ptr;
    }
    class AssertionChecker {
        typedef AssertionChecker Self;
    public:
        static void reg(Self& checker) { currentChecker() = &checker; }
        static Self& get() { return *currentChecker(); }

        virtual void before(const char* file, int line) =0;
        virtual void after() =0;
        virtual void check(const AssertionInfo& info) =0;
    };
    //@nonl
    //@-node:gcross.20090108164821.21:class AssertionChecker
    //@+node:gcross.20090108164821.22:struct CheckerGuard
    // make sure before() and after() are called
    struct CheckerGuard {
        CheckerGuard(const char* file, int line)
            { AssertionChecker::get().before(file, line); }
        ~CheckerGuard() { AssertionChecker::get().after(); }
    };
    //@nonl
    //@-node:gcross.20090108164821.22:struct CheckerGuard
    //@+node:gcross.20090108164821.23:checking functions

    template <typename T1, typename T2>
    inline bool equalValue(const T1& expected, const T2& actual)
    {
        return expected == actual;
    }
    template <typename T1, typename T2>
    inline AssertionInfo equalValueInfo(const T1& expected, const T2& actual)
    {
        if (equalValue(expected, actual)) return AssertionInfo(true);

        std::ostringstream oo;
        oo << "expected <" << expected << ">, but was <" << actual << ">";
        return AssertionInfo(false, oo.str());
    }

    template <typename T1, typename T2, typename T3>
    inline bool closeValue(const T1& expected, const T2& actual, const T3& precision)
    {
        return !(expected + precision < actual)
            && !(actual + precision < expected);
    }
    template <typename T1, typename T2, typename T3>
    inline AssertionInfo closeValueInfo(const T1& expected, const T2& actual, const T3& precision)
    {
        if (closeValue(expected, actual, precision)) return AssertionInfo(true);

        std::ostringstream oo;
        oo << "expected <" << expected << ">, but was <" << actual << ">, not close enough with precision <" << precision << ">";
        return AssertionInfo(false, oo.str());
    }
    //@-node:gcross.20090108164821.23:checking functions
    //@-node:gcross.20090108164821.19:Assertions
    //@+node:gcross.20090108164821.24:miscelleneous
    int printHelp(const std::string& execFile);
    int listCases();
    std::string getFormat(const std::string& style);
    int runTest(const std::string& errorFormat);
    int defaultMain(int argc, char* argv[]);
    //@nonl
    //@-node:gcross.20090108164821.24:miscelleneous
    //@-others


} // namespace unit_minus
//@-node:gcross.20090108164821.10:namespace unit_minus
//@+node:gcross.20090108164821.25:Macros
//@+node:gcross.20090108164821.26:suite macros
// wrapper code for Suite<>
// a test suite should be a Suite<>
#define testSuitePrefix(thisSuite, upperSuite, prefix)                        \
class thisSuite: public unit_minus::Suite<thisSuite, upperSuite> {            \
    public: virtual const char* caption() const { return #thisSuite; }        \
};                                                                            \
const bool prefix##thisSuite                                                  \
    = unit_minus::suiteInstance<thisSuite>().addToUpper(); // 3.6.2 foot note 31

#define subSuite(thisSuite, upperSuite)                                       \
testSuitePrefix(thisSuite, upperSuite, added_)

#define testSuite(thisSuite) subSuite(thisSuite, unit_minus::Root)
//@nonl
//@-node:gcross.20090108164821.26:suite macros
//@+node:gcross.20090108164821.27:test case macros
// wrapper code for Case<>
// a test case should be a Case<>
#define testCasePrefix(thisCase, upperSuite, prefix)                          \
class thisCase: public unit_minus::Case<thisCase, upperSuite> {               \
public:                                                                       \
    virtual const char* caption() const { return #thisCase; }                 \
private:                                                                      \
    virtual const char* file() const { return __FILE__; }                     \
    virtual int line() const { return __LINE__; }                             \
    virtual void test();                                                      \
};                                                                            \
const bool prefix##thisCase                                                   \
    = unit_minus::caseInstance<thisCase>().addToUpper();                      \
inline void thisCase::test()

#define testCase(thisCase, upperSuite)                                        \
testCasePrefix(thisCase, upperSuite, added_)
//@nonl
//@-node:gcross.20090108164821.27:test case macros
//@+node:gcross.20090108164821.28:assertion macros
// test assertion
// param: expression to test
// true for pass, false for failure
#define assertTrue(opt_expression)                                            \
{                                                                             \
    unit_minus::CheckerGuard opt_tempGard(__FILE__, __LINE__);                \
    unit_minus::AssertionInfo opt_info(opt_expression);                       \
    opt_info.rawMessage("Assertion failed: <" #opt_expression ">");           \
    unit_minus::AssertionChecker::get().check(opt_info);                      \
}

// test assertion
// param: values whose equality is to be tested
#define assertEqual(A,B)                                            \
{                                                                             \
    unit_minus::CheckerGuard opt_tempGard(__FILE__, __LINE__);                \
    unit_minus::AssertionInfo opt_info = unit_minus::equalValueInfo(A,B);     \
    unit_minus::AssertionChecker::get().check(opt_info);                      \
}

// test assertion
// assume failure when executed
// param: error message
#define assertNoArrive(opt_message)                                           \
{                                                                             \
    unit_minus::CheckerGuard opt_tempGard(__FILE__, __LINE__);                \
    unit_minus::AssertionInfo opt_info(false, (opt_message));                 \
    opt_info.rawMessage(opt_message);                                         \
    unit_minus::AssertionChecker::get().check(opt_info);                      \
}
//@nonl
//@-node:gcross.20090108164821.28:assertion macros
//@+node:gcross.20090108164821.29:class FailureInfo
// note failure information and transfer it out
class FailureInfo {
private:
    const char* m_file;
    int m_line;
    std::string m_desc;
public:
    FailureInfo(const char* filep, int linep, const std::string& descp)
        : m_file(filep), m_line(linep), m_desc(descp) {}
    const char* file() const { return m_file; }
    int line() const { return m_line; }
    const std::string& desc() const { return m_desc; }
};
//@nonl
//@-node:gcross.20090108164821.29:class FailureInfo
//@+node:gcross.20090108164821.36:default main
#define useDefaultMain int main(int argc, char* argv[]) { unit_minus::defaultMain(argc,argv); }
//@-node:gcross.20090108164821.36:default main
//@-node:gcross.20090108164821.25:Macros
//@-others


#endif // unitmm_H_opt123
//@-node:gcross.20090108164821.5:@thin unit--.h
//@-leo
