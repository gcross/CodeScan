//@+leo-ver=4-thin
//@+node:gcross.20090108164821.4:@thin unit--.C
//@@language cplusplus
//@@tabwidth -4
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

#include <iostream>
#include <string>
#include <ctime>
#include "unit--.h"

//@+others
//@+node:gcross.20090108164821.41:anonymous namespace
namespace {
// basic Log implementation
using unit_minus::SuiteBase;
using unit_minus::CaseBase;

//@+others
//@+node:gcross.20090108164821.30:Runners
//@+node:gcross.20090108164821.31:class EnhancedRunner
// test all cases, and output error message to std::cout
class EnhancedRunner: public unit_minus::Runner {

    enum {INDENT_SIZE = 4};

private:
    typedef EnhancedRunner Self;
    unsigned m_failureCount;
    unsigned m_totalCount;
    unsigned m_indent;

    class Checker: public unit_minus::AssertionChecker {
        const char* m_file;
        int m_line;
    public:
        virtual void before(const char* file, int line)
        {
            m_file = file;
            m_line = line;
        }
        virtual void after() {}
        virtual void check(const unit_minus::AssertionInfo& info)
        {
            if (info.successful()) return;
            throw FailureInfo(
                m_file, m_line,
                (info.hasMessage() ? info.message() : info.rawMessage()));
        }
    };

    Checker m_checker;
    std::string m_errorFormat;
public:
    EnhancedRunner(const std::string& errorFormat)
        : m_failureCount(0), m_totalCount(0), m_indent(0)
    {
        unit_minus::AssertionChecker::reg(m_checker);
        m_errorFormat = errorFormat;
    }
    // do not care about suites
    virtual void enter(SuiteBase& suite) {
        using namespace std;
        cout << string(INDENT_SIZE * m_indent, ' ');
        cout << "\033[43m" << suite.caption() << ":\033[0m" << endl;
        ++m_indent;
    }

    virtual void exit(SuiteBase&) {
        --m_indent;
    }

    virtual void check(CaseBase& aCase)
    {
        using namespace std;
        try {
            cout << string(INDENT_SIZE * m_indent, ' ');
            cout << "\033[1;33m" << aCase.caption() << "... \033[0m" << flush;
            aCase.test();
            pass(aCase);
            return;
        }
        // assertion failed
        catch (FailureInfo& info) {
            fail(aCase, info);
            return;
        }
        // some kind of unexpected exception
        catch (std::exception& e) {
            std::ostringstream mess;
            mess << "unexpected exception with description:\n";
            mess << e.what();
            FailureInfo info(aCase.file(), aCase.line(), mess.str());
            fail(aCase, info);
            return;
        }
        catch (...) {
            std::ostringstream mess;
            mess << "unexpected unknown exception";
            FailureInfo info(aCase.file(), aCase.line(), mess.str());
            fail(aCase, info);
            return;
        }
    }

    // a test case failed
    virtual void fail(CaseBase& aCase, FailureInfo& info)
    {
        using namespace std;
        cout << "\033[1;31m FAIL :-( \033[0m" << endl;
        cout << string(INDENT_SIZE * (m_indent+1), ' ');
        cout << "\033[1;31m" << errorMessage(info) << "\033[0m" << endl;

        ++m_failureCount;
        ++m_totalCount;
    }
    std::string errorMessage(FailureInfo& info) const
    {
        using namespace std;
        ostringstream oo;
        for (string::size_type i = 0; i < m_errorFormat.length(); ++i) {
            if ('_' != m_errorFormat[i]) {
                oo << m_errorFormat[i];
                continue;
            }

            ++i;
            if (m_errorFormat.length() == i) break;
            if ('f' == m_errorFormat[i]) oo << info.file();
            else if ('l' == m_errorFormat[i]) oo << info.line();
            else if ('m' == m_errorFormat[i]) oo << info.desc();
            else oo << m_errorFormat[i];
        }
        return oo.str();
    }

    // a test case passed
    virtual void pass(CaseBase&)
    {
        using namespace std;
        ++m_totalCount;
        cout << "\033[1;32m PASS :-) \033[0m" << endl;
    }

    bool ok() const
    {
        return 0 == m_failureCount;
    }
    // print summery of whole test
    void printSummary() const
    {
        using namespace std;
        cout << "\n";
        if (0 == m_failureCount) {
            cout << "OK" << endl;
        }
        else {
            cout << m_failureCount << " case";
            if (m_failureCount > 1) cout << "s";
            cout << " failed" << endl;
        }

        cout << "Total " << m_totalCount << " test case";
        if (m_totalCount > 1) cout << "s";
        cout << endl;
    }
};
//@-node:gcross.20090108164821.31:class EnhancedRunner
//@+node:gcross.20090108164821.32:class StdRunner
// test all cases, and output error message to std::cout
class StdRunner: public unit_minus::Runner {
private:
    typedef StdRunner Self;
    unsigned m_failureCount;
    unsigned m_totalCount;

    class Checker: public unit_minus::AssertionChecker {
        const char* m_file;
        int m_line;
    public:
        virtual void before(const char* file, int line)
        {
            m_file = file;
            m_line = line;
        }
        virtual void after() {}
        virtual void check(const unit_minus::AssertionInfo& info)
        {
            if (info.successful()) return;
            throw FailureInfo(
                m_file, m_line,
                (info.hasMessage() ? info.message() : info.rawMessage()));
        }
    };

    Checker m_checker;
    std::string m_errorFormat;
public:
    StdRunner(const std::string& errorFormat)
        : m_failureCount(0), m_totalCount(0)
    {
        unit_minus::AssertionChecker::reg(m_checker);
        m_errorFormat = errorFormat;
    }
    // do not care about suites
    virtual void enter(SuiteBase&) {}
    virtual void exit(SuiteBase&) {}

    virtual void check(CaseBase& aCase)
    {
        try {
            aCase.test();
            pass(aCase);
            return;
        }
        // assertion failed
        catch (FailureInfo& info) {
            fail(aCase, info);
            return;
        }
        // some kind of unexpected exception
        catch (std::exception& e) {
            std::ostringstream mess;
            mess << "unexpected exception with description:\n";
            mess << e.what();
            FailureInfo info(aCase.file(), aCase.line(), mess.str());
            fail(aCase, info);
            return;
        }
        catch (...) {
            std::ostringstream mess;
            mess << "unexpected unknown exception";
            FailureInfo info(aCase.file(), aCase.line(), mess.str());
            fail(aCase, info);
            return;
        }
    }

    // a test case failed
    virtual void fail(CaseBase& aCase, FailureInfo& info)
    {
        using namespace std;
        cout << "X\n";
        cout << aCase.caption() << "\n";
        cout << errorMessage(info) << "\n";

        ++m_failureCount;
        ++m_totalCount;
    }
    std::string errorMessage(FailureInfo& info) const
    {
        using namespace std;
        ostringstream oo;
        for (string::size_type i = 0; i < m_errorFormat.length(); ++i) {
            if ('_' != m_errorFormat[i]) {
                oo << m_errorFormat[i];
                continue;
            }

            ++i;
            if (m_errorFormat.length() == i) break;
            if ('f' == m_errorFormat[i]) oo << info.file();
            else if ('l' == m_errorFormat[i]) oo << info.line();
            else if ('m' == m_errorFormat[i]) oo << info.desc();
            else oo << m_errorFormat[i];
        }
        return oo.str();
    }

    // a test case passed
    virtual void pass(CaseBase&)
    {
        using namespace std;
        ++m_totalCount;
        cout << "." << flush;
    }

    bool ok() const
    {
        return 0 == m_failureCount;
    }
    // print summery of whole test
    void printSummary() const
    {
        using namespace std;
        cout << "\n";
        if (0 == m_failureCount) {
            cout << "OK" << endl;
        }
        else {
            cout << m_failureCount << " case";
            if (m_failureCount > 1) cout << "s";
            cout << " failed" << endl;
        }

        cout << "Total " << m_totalCount << " test case";
        if (m_totalCount > 1) cout << "s";
        cout << endl;
    }
};
//@nonl
//@-node:gcross.20090108164821.32:class StdRunner
//@+node:gcross.20090108164821.33:class ListBuilder
// list all test cases with a tree structure, but don't do any test
class ListBuilder: public unit_minus::Runner {
private:
    enum {INDENT_SIZE = 4};
    unsigned m_indent;
    typedef ListBuilder Self;
public:
    ListBuilder(): m_indent(0) {}
    virtual void enter(SuiteBase& suite)
    {
        using namespace std;
        cout << string(INDENT_SIZE * m_indent, ' ');
        cout << suite.caption() << endl;
        ++m_indent;
    }
    virtual void exit(SuiteBase&)
    {
        --m_indent;
    }

    virtual void check(CaseBase& ca)
    {
        using namespace std;
        cout << string(INDENT_SIZE * m_indent, ' ');
        cout << ca.caption() << "\n";
    }
};

//@-node:gcross.20090108164821.33:class ListBuilder
//@-node:gcross.20090108164821.30:Runners
//@-others

}
//@nonl
//@-node:gcross.20090108164821.41:anonymous namespace
//@+node:gcross.20090108164821.34:namespace unit_minus
namespace unit_minus {

//@+others
//@+node:gcross.20090108164821.37:printHelp
int printHelp(const std::string& execFile)
{
    using namespace std;
    cout
        << "usage:\n"
        << execFile << " [ <style> | -h | -l ]\n"
        << "\n"
        << "<style>       style of assertion failure report\n"
        << "    gnu       for GNU Emacs (default)\n"
        << "    vc        for MS Visual C++\n"
        << "    format\"<format string>\"\n"
        << "              there is no space between format and <format string>\n"
        << "              user defined style, there are some conversions available:\n"
        << "              _f for source file name,\n"
        << "              _l (lower case L) for line number, and\n"
        << "              _m for a (perhaps) descriptive message\n"
        << "    example:  gnu style is equivalent to format\"_f:_l: _m\"\n"
        << "\n"
        << "-h, --help    print this help message\n"
        << "-l, --list    list all test cases and suites in tree structure\n"
        << flush;
    return 0;
}
//@nonl
//@-node:gcross.20090108164821.37:printHelp
//@+node:gcross.20090108164821.38:listCases
int listCases()
{
    using namespace unit_minus;
    ListBuilder listBuilder;
    suiteInstance<Root>().run(listBuilder);
    return 0;
}
//@nonl
//@-node:gcross.20090108164821.38:listCases
//@+node:gcross.20090108164821.39:getFormat
std::string getFormat(const std::string& style)
{
    using namespace std;
    if ("gnu" == style) return "_f:_l: _m";
    if ("vc"  == style) return "_f(_l) : error: _m";

    const string format = "format";
    if (style.length() > format.length()
        && style.substr(0, format.length()) == format
    ) {
        return style.substr(format.length(), string::npos);
    }
    return getFormat("gnu");
}
//@nonl
//@-node:gcross.20090108164821.39:getFormat
//@+node:gcross.20090108164821.40:runTest
int runTest(const std::string& errorFormat)
{
    using namespace unit_minus;
    using namespace std;

    EnhancedRunner runner(errorFormat);
    time_t t0 = time(0);
    suiteInstance<Root>().run(runner);
    double period = difftime(time(0), t0);
    runner.printSummary();
    cout << difftime(time(0), t0) << " sec";
    if (period > 1.0) cout << "s";
    cout <<"." << endl;
    return runner.ok() ? 0 : 1;
}
//@nonl
//@-node:gcross.20090108164821.40:runTest
//@+node:gcross.20090108164821.35:defaultMain
int defaultMain(int argc, char* argv[])
{
    using namespace unit_minus;
    using namespace std;

    if (argc < 2) {
        return runTest(getFormat("gnu"));
    }

    if (argc > 2 || string(argv[1]) == "-h" || string(argv[1]) == "--help") {
        return printHelp(argv[0]);
    }

    if (string(argv[1]) == "-l" || string(argv[1]) == "--list") {
        return listCases();
    }

    return runTest(getFormat(argv[1]));
}
//@-node:gcross.20090108164821.35:defaultMain
//@-others

}
//@-node:gcross.20090108164821.34:namespace unit_minus
//@-others



//@-node:gcross.20090108164821.4:@thin unit--.C
//@-leo
