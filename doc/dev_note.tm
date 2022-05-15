<TeXmacs|2.1.1>

<style|<tuple|generic|chinese>>

<\body>
  <doc-data|<doc-title|PicoScheme\<#5F00\>\<#53D1\>\<#5C0F\>\<#8BB0\>>>

  <section|\<#5199\>\<#5728\>\<#524D\>\<#9762\>\<#7684\>\<#80E1\>\<#8A00\>\<#4E71\>\<#8BED\>>

  PicoScheme\<#6700\>\<#521D\>\<#7531\><hlink|arichel|https://github.com/arichel/PicoScheme>\<#5F00\>\<#53D1\>\<#FF0C\>\<#91C7\>\<#7528\><verbatim|std::variant>\<#4F5C\>\<#4E3A\>\<#57FA\>\<#7840\>\<#6570\>\<#636E\>\<#7ED3\>\<#6784\>\<#FF0C\>\<#5B9E\>\<#73B0\>\<#4E86\>lisp-style
  macro\<#FF0C\>\<#90E8\>\<#5206\>\<#5B9E\>\<#73B0\>\<#4E86\><verbatim|call/cc>\<#FF08\>\<#4EC5\>\<#5B9E\>\<#73B0\>\<#4E86\>simple
  secape procedure\<#FF0C\>\<#65E0\>\<#6CD5\>\<#4FDD\>\<#5B58\>continuation\<#FF09\>\<#3002\>\<#5728\>GitHub\<#4E0A\>\<#4F17\>\<#591A\>\<#90FD\>toy
  scheme\<#5B9E\>\<#73B0\>\<#4E2D\>\<#FF0C\>\<#51E0\>\<#4E4E\>\<#6CA1\>\<#6709\>scheme\<#5B9E\>\<#73B0\><verbatim|macro>\<#548C\><verbatim|call/cc>\<#3002\>PicoScheme\<#7684\>\<#5B9E\>\<#73B0\>\<#7B97\>\<#662F\>\<#6BD4\>\<#8F83\>\<#5B8C\>\<#6574\>\<#7684\>\<#FF0C\>\<#4EE3\>\<#7801\>\<#7684\>\<#7ED3\>\<#6784\>\<#4E5F\>\<#6E05\>\<#6670\>\<#FF0C\>\<#6700\>\<#91CD\>\<#8981\>\<#7684\>\<#662F\>\<#53EF\>\<#4EE5\>\<#5728\>MSVC\<#4E0A\>\<#7F16\>\<#8BD1\>\<#3002\>

  \<#6211\>\<#9009\>\<#62E9\>PicoScheme\<#4F5C\>\<#4E3A\><TeXmacs>\<#65B0\>\<#7684\>Scheme\<#5B9E\>\<#73B0\>\<#FF0C\>\<#6700\>\<#91CD\>\<#8981\>\<#7684\>\<#539F\>\<#56E0\>\<#662F\>\<#6211\>\<#5E0C\>\<#671B\>\<#6709\>\<#4E00\>\<#4E2A\>MSVC\<#7F16\>\<#8BD1\>\<#7684\><TeXmacs>\<#3002\>\<#5728\>\<#51E0\>\<#5E74\>\<#524D\>\<#FF0C\>\<#6211\>\<#66FE\>\<#5C1D\>\<#8BD5\>\<#8FC7\>\<#7528\>MSVC\<#7F16\>\<#8BD1\>Guile\<#FF0C\>\<#4F46\>\<#6700\>\<#7EC8\>\<#5931\>\<#8D25\>\<#4E86\>\<#3002\>\<#8FD9\>\<#4E2A\>\<#5C1D\>\<#8BD5\>\<#53EF\>\<#4EE5\>\<#770B\>\<#6211\>\<#4E4B\>\<#524D\>\<#5199\>\<#7684\><hlink|\<#90AE\>\<#4EF6\>|https://lists.gnu.org/archive/html/texmacs-dev/2020-08/msg00001.html>\<#3002\>

  <TeXmacs>\<#7684\>\<#9ED8\>\<#8BA4\>Scheme\<#7528\>\<#7684\>\<#662F\>Guile
  1.8\<#3002\>\<#4F46\>\<#662F\>\<#FF0C\>\<#968F\>\<#7740\>\<#5404\>\<#5927\>linux\<#53D1\>\<#884C\>\<#7248\>\<#7684\>\<#6E90\>\<#79FB\>\<#9664\>\<#4E86\>Guile
  1.8\<#7684\>\<#5305\>\<#FF0C\>\<#5728\>linux\<#4E0A\>\<#7F16\>\<#8BD1\><TeXmacs>\<#4E5F\>\<#6E10\>\<#6E10\>\<#53D8\>\<#5F97\>\<#4E0D\>\<#90A3\>\<#4E48\>\<#65B9\>\<#4FBF\>\<#3002\>\<#4E8E\>\<#662F\>\<#4E4E\>\<#FF0C\>\<#9759\>\<#6001\>\<#7F16\>\<#8BD1\>Guile\<#FF0C\>\<#6362\>\<#5176\>\<#4ED6\>\<#7684\>Scheme\<#5B9E\>\<#73B0\>\<#6210\>\<#4E86\>\<#7F16\>\<#8BD1\>\<#548C\>\<#53D1\>\<#5E03\><TeXmacs>\<#4E0D\>\<#5F97\>\<#4E0D\>\<#9762\>\<#5BF9\>\<#7684\>\<#4E8B\>\<#60C5\>\<#3002\>

  \<#6211\>\<#6700\>\<#521D\>\<#60F3\>\<#81EA\>\<#5DF1\>\<#5199\>\<#4E86\>\<#4E00\>\<#4E2A\>Scheme\<#FF0C\>\<#7EAF\>\<#7CB9\>\<#662F\>\<#56E0\>\<#4E3A\>\<#5728\>\<#5B66\>\<#4E60\>SICP\<#65F6\>\<#624B\>\<#75D2\>\<#FF0C\>\<#5E76\>\<#6CA1\>\<#6709\>\<#8003\>\<#8651\>\<#5F88\>\<#591A\>\<#4E1C\>\<#897F\>\<#FF0C\>\<#751A\>\<#81F3\>\<#4E00\>\<#76F4\>\<#90FD\>\<#6CA1\>\<#6709\>\<#52A8\>\<#624B\>\<#5199\>\<#4E0B\>\<#7B2C\>\<#4E00\>\<#884C\>\<#4EE3\>\<#7801\>\<#3002\>\<#76F4\>\<#5230\>\<#6709\>\<#4E00\>\<#5929\>\<#FF0C\>\<#6211\>\<#53C2\>\<#4E0E\>Mogan\<#7684\>\<#5F00\>\<#53D1\>\<#FF0C\>\<#53C8\>\<#60F3\>\<#5230\>\<#4E86\>\<#4E4B\>\<#524D\>\<#5012\>\<#817E\>MSVC\<#7248\>\<#672C\>\<#7684\>Guile\<#3002\>\<#4E8E\>\<#662F\>\<#FF0C\>\<#5E72\>\<#8106\>\<#81EA\>\<#5DF1\>\<#5F04\>\<#4E00\>\<#4E2A\>Scheme\<#FF0C\>\<#5E76\>\<#6784\>\<#5EFA\>MSVC\<#7248\>\<#672C\>\<#7684\><TeXmacs>\<#3002\>

  <section|PicoScheme\<#8BBE\>\<#8BA1\>\<#76EE\>\<#6807\>>

  <\itemize>
    <item>\<#5B66\>\<#4E60\>SICP

    \<#80FD\>\<#591F\>\<#6B63\>\<#5E38\>\<#8FD0\>\<#884C\>SICP\<#7684\>\<#5143\>\<#5FAA\>\<#73AF\>\<#6C42\>\<#503C\>\<#5668\>\<#548C\>\<#663E\>\<#793A\>\<#63A7\>\<#5236\>\<#6C42\>\<#503C\>\<#5668\>

    <item>\<#4F5C\>\<#4E3A\>MSVC\<#7248\>\<#672C\><TeXmacs>\<#9ED8\>\<#8BA4\>scheme\<#5B9E\>\<#73B0\>

    \<#4EE5\>guile\<#7684\>\<#6807\>\<#51C6\>\<#FF0C\>\<#652F\>\<#6301\><TeXmacs>\<#8FD0\>\<#884C\>

    <item>\<#5B66\>\<#4E60\>\<#7F16\>\<#8BD1\>\<#539F\>\<#7406\>

    \<#80FD\>\<#591F\>\<#8FD0\>\<#884C\>\<#5728\>\<#9F99\>\<#4E66\>\<#7684\>\<#90A3\>\<#4E2A\>\<#5C0F\>\<#673A\>\<#5668\>\<#4E0A\>\<#FF0C\>\<#65B9\>\<#4FBF\>\<#5B66\>\<#4E60\>\<#7F16\>\<#8BD1\>\<#4F18\>\<#5316\>

    <item>\<#5B9E\>\<#73B0\>\<#6D4F\>\<#89C8\>\<#5668\>\<#FF08\>WebAssembly\<#FF09\>\<#4E0A\>\<#8FD0\>\<#884C\>

    \<#63A2\>\<#7D22\><TeXmacs>\<#4E0A\>\<#8FD0\>\<#884C\>\<#7684\>\<#53EF\>\<#80FD\>\<#6027\>
  </itemize>

  <section|PicoScheme\<#5F00\>\<#53D1\>\<#8FC7\>\<#7A0B\>>

  <TeXmacs>\<#7528\>\<#5230\>\<#7684\>Scheme\<#7279\>\<#6027\>

  <\itemize>
    <item>macro

    <item>HashMap

    <item>\<#53EF\>\<#4EE5\>\<#5B58\>\<#653E\><verbatim|void*>\<#6307\>\<#9488\>

    <item>Module

    <item><verbatim|call-with-values>
  </itemize>

  PicoScheme\<#5DF2\>\<#7ECF\>\<#5B9E\>\<#73B0\>\<#4E86\>\<#7684\>list-style
  macro\<#5DF2\>\<#7ECF\>\<#591F\>\<#7528\>\<#4E86\>\<#3002\>

  HashMap\<#548C\><verbatim|void*>\<#597D\>\<#529E\>\<#FF0C\>\<#52A0\>\<#4E24\>\<#4E2A\>\<#7C7B\>\<#5C31\>\<#641E\>\<#5B9A\>\<#4E86\>\<#3002\>

  Module\<#7A0D\>\<#5FAE\>\<#9EBB\>\<#70E6\>\<#4E86\>\<#4E9B\>\<#FF0C\>\<#6211\>\<#4E5F\>\<#52A0\>\<#4E86\>\<#4E2A\>\<#7C7B\>\<#505A\>\<#9694\>\<#79BB\>\<#FF0C\>\<#6682\>\<#65F6\>\<#8FD8\>\<#6CA1\>\<#51FA\>\<#95EE\>\<#9898\>\<#3002\>

  <verbatim|call-with-values>\<#5C31\>\<#9EBB\>\<#70E6\>\<#4E86\>\<#FF0C\>\<#7528\>\<#5230\>\<#4E86\><verbatim|contiuation>\<#8FD9\>\<#4E2A\>\<#7279\>\<#6027\>\<#FF0C\>\<#76EE\>\<#524D\>\<#6839\>\<#672C\>\<#5C31\>\<#505A\>\<#4E0D\>\<#4E86\>\<#3002\>

  \<#4E8E\>\<#662F\>\<#600E\>\<#4E48\>\<#5B9E\>\<#73B0\>continuation\<#5C31\>\<#6210\>\<#4E86\>\<#5F53\>\<#52A1\>\<#4E4B\>\<#6025\>\<#3002\>

  <subsection|\<#5B9E\>\<#73B0\>compiler>

  <subsubsection|\<#7B2C\>\<#4E00\>\<#9636\>\<#6BB5\>
  \<#7528\>c++\<#91CD\>\<#5199\>SICP\<#4E2D\>\<#7684\>compiler>

  \<#5B9E\>\<#73B0\>compiler\<#7684\>\<#521D\>\<#8877\>\<#662F\>\<#4E3A\>\<#4E86\>\<#5B9E\>\<#73B0\>continuation\<#7279\>\<#6027\>\<#3002\>\<#5728\>\<#770B\>\<#5230\>SICP\<#7B2C\>5\<#7AE0\>\<#7684\>\<#65F6\>\<#5019\>\<#FF0C\>\<#6211\>\<#89C9\>\<#5F97\>\<#53EF\>\<#4EE5\>\<#628A\>\<#8FD9\>\<#4E2A\>\<#5BC4\>\<#5B58\>\<#673A\>\<#5668\>\<#7528\><verbatim|c++>\<#5199\>\<#4E00\>\<#904D\>\<#FF0C\>\<#4F5C\>\<#4E3A\>\<#7B2C\>\<#4E00\>\<#7248\>\<#7684\>compiler\<#3002\>

  \<#5BC4\>\<#5B58\>\<#673A\>\<#5668\>\<#4E2D\>\<#7684\>instruction

  <\itemize>
    <item>The name of a data-path button to push to assign a value to a
    register.

    <item>A <verbatim|test> instruction, that performs a specified test.

    <item>A conditional branch (<verbatim|branch> instruction) to a location
    indicated by a controller label, based on the result of the previous
    test. If the test is false, the controller should contine with the next
    instruction in the sequence. Otherwise, the controller should continue
    with the instruction after the label.

    <item>An unconditional branch (<verbatim|goto> instruction) naming a
    controller label at which to continue execution.
  </itemize>

  instruction\<#8BED\>\<#6CD5\>

  <\scm-code>
    (assign \<less\>register-name\<gtr\> (reg \<less\>register-name\<gtr\>))

    (assign \<less\>register-name\<gtr\> (const
    \<less\>constant-value\<gtr\>))

    (assign \<less\>register-name\<gtr\> (op \<less\>operation-name\<gtr\>)
    \<less\>input_1\<gtr\> ... \<less\>input_n\<gtr\>)

    (perform (op \<less\>operation-name\<gtr\>) \<less\>input_1\<gtr\> ...
    \<less\>input_n\<gtr\>)

    (test (op \<less\>operation-name\<gtr\>) \<less\>input_1\<gtr\> ...
    \<less\>input_n\<gtr\>)

    (branch (label \<less\>label-name\<gtr\>))

    (goto (label \<less\>label-name\<gtr\>))

    ;;; label

    (assign \<less\>register-name\<gtr\> (label \<less\>label-name\<gtr\>))

    (goto (reg \<less\>register-name\<gtr\>))

    ;;; stack

    (save \<less\>register-name\<gtr\>)

    (restore \<less\>register-name\<gtr\>)

    ;;; contiuation

    (cont)
  </scm-code>

  \<#5BC4\>\<#5B58\>\<#5668\>

  \<#76EE\>\<#524D\>\<#7528\>\<#5230\>\<#4E86\>5\<#4E2A\>\<#5BC4\>\<#5B58\>\<#5668\>

  <\itemize>
    <item>env

    \<#5F53\>\<#524D\>\<#51FD\>\<#6570\>\<#7684\>\<#6267\>\<#884C\>\<#73AF\>\<#5883\>

    <item>proc

    \<#5F53\>\<#524D\>\<#6267\>\<#884C\>\<#7684\>\<#51FD\>\<#6570\>

    <item>val

    \<#5F53\>\<#524D\>\<#51FD\>\<#6570\>\<#6267\>\<#884C\>\<#7684\>\<#7ED3\>\<#679C\>

    <item>argl

    \<#5F53\>\<#524D\>\<#51FD\>\<#6570\>\<#6267\>\<#884C\>\<#7684\>\<#53C2\>\<#6570\>

    <item>continue

    \<#5F53\>\<#524D\>\<#51FD\>\<#6570\>\<#6267\>\<#884C\>\<#5B8C\>\<#6BD5\>\<#540E\>\<#FF0C\>\<#4E0B\>\<#4E00\>\<#6761\>\<#6307\>\<#4EE4\>\<#7684\>\<#4F4D\>\<#7F6E\>
  </itemize>

  <subsubsection|\<#7B2C\>\<#4E8C\>\<#9636\>\<#6BB5\>
  \<#7F16\>\<#8BD1\>\<#4F18\>\<#5316\>>

  \;

  <subsection|\<#5B9E\>\<#73B0\>continuation>

  \<#5728\>\<#770B\>\<#5230\><hlink|Continuation
  Implementation\<#8FD9\>\<#7BC7\>\<#6587\>\<#7AE0\>|https://wiki.c2.com/?ContinuationImplementation>\<#4E4B\>\<#524D\>\<#FF0C\>\<#6211\>\<#5176\>\<#5B9E\>\<#5BF9\>\<#600E\>\<#4E48\>\<#5B9E\>\<#73B0\>continuation\<#5168\>\<#9760\>\<#8BD5\>\<#FF0C\>\<#5C31\>\<#662F\>\<#60F3\>\<#5230\>\<#4E00\>\<#4E2A\>\<#65B9\>\<#6CD5\>\<#FF0C\>\<#5C31\>\<#53BB\>\<#5199\>\<#4EE3\>\<#7801\>\<#5B9E\>\<#73B0\>\<#4E00\>\<#4E0B\>\<#FF0C\>\<#770B\>\<#770B\>\<#80FD\>\<#4E0D\>\<#80FD\>\<#884C\>\<#3002\>

  \<#6587\>\<#7AE0\>\<#4E2D\>\<#4ECB\>\<#7ECD\>\<#4E86\>\<#4EE5\>\<#4E0B\>\<#51E0\>\<#79CD\>\<#5B9E\>\<#73B0\>\<#65B9\>\<#6CD5\>\<#FF1A\>

  <\itemize>
    <item>Heap allocation of ActivationRecords

    Each activation record is a separate heap object, chained together by a
    pointer to its caller. Continuation capture saves the pointer to the
    current record, which prevents it and all stack frames under it from
    being garbage collected. Continuation reinstatement restores the PC,
    registers, and current record.

    <item>Copying whole stacks

    Stack frames are allocated normally, but when a continuation is captured,
    the whole stack is copied and saved in the continuation. Continuation
    reinstatement restores the register state and sets the stack and frame
    pointers to the continuation's stack. The old stack can be deallocated or
    freed by the garbage collector.

    <item>Segmented stacks

    The stack is allocated in large chunks, with an overflow handler to
    allocate a new chunk as necessary (the virtual memory hardware can handle
    this, by mapping in a non-writable page at the address past the stack).
    Continuation capture shortens the stack and saves a pointer to the
    previous portion.\ 

    <item>ContinuationPassingStyle

    Convert the whole program to CPS. The continuation is automatically
    available within every expression.\ 
  </itemize>

  \<#5728\>\<#7528\>\<#5BC4\>\<#5B58\>\<#673A\>\<#5668\>\<#5B9E\>\<#73B0\>continuation\<#65F6\>\<#FF0C\>\<#6211\>\<#53C8\>\<#627E\>\<#5230\>\<#4E00\>\<#7BC7\>\<#8BBA\>\<#6587\>\<#FF0C\>\<#6BD4\>\<#8F83\>\<#8001\>\<#4E86\>\<#3002\>

  <hlink|Implementation Strategies for First-Class
  Continuations|https://link.springer.com/article/10.1023/A:1010016816429>

  \<#76EE\>\<#524D\>PicoScheme\<#7684\>continuation\<#5B9E\>\<#73B0\>\<#7ECF\>\<#5386\>\<#4EE5\>\<#4E0B\>\<#51E0\>\<#4E2A\>\<#9636\>\<#6BB5\>

  <subsubsection|\<#7B2C\>\<#4E00\>\<#9636\>\<#6BB5\>
  \<#539F\>\<#4F5C\>\<#8005\>arichel\<#5B9E\>\<#73B0\>\<#7684\>simple secape
  procedure>

  \<#5177\>\<#4F53\>\<#505A\>\<#6CD5\>\<#662F\>

  1. \<#5B9A\>\<#4E49\>\<#4E00\>\<#4E2A\>\<#5F02\>\<#5E38\>

  <\cpp-code>
    struct continuation_exception {

    \ \ \ \ continuation_exception(Scheme& scm, const
    std::vector\<less\>Cell\<gtr\>& args) {

    \ \ \ \ \ \ \ \ if (args.empty())

    \ \ \ \ \ \ \ \ \ \ \ \ return;

    \ \ \ \ \ \ \ \ continuation = args.size() != 1 ? primop::list(scm, args)
    : args[0];

    \ \ \ \ }

    \ \ \ \ Cell continuation = none;

    };
  </cpp-code>

  2. \<#6784\>\<#9020\>\<#4E00\>\<#4E2A\><verbatim|lambda>\<#629B\>\<#51FA\>\<#8FD9\>\<#4E2A\>\<#5F02\>\<#5E38\>

  <\cpp-code>
    auto lambda = [](Scheme& scm, const SymenvPtr&, const
    std::vector\<less\>Cell\<gtr\>& args) -\<gtr\> Cell {

    \ \ \ \ throw continuation_exception{ scm, args };

    };
  </cpp-code>

  3. continuation\<#8C03\>\<#7528\>\<#65F6\>\<#FF0C\>\<#5C31\>\<#8C03\>\<#7528\>\<#8FD9\>\<#4E2A\>lambda

  <\cpp-code>
    try {

    \ \ \ \ // call (f c)

    \ \ \ \ auto f = args[0];

    \ \ \ \ auto c = scm.function(senv, std::move(lambda))

    \ \ \ \ return pscm::apply(scm, senv, f, c);

    } catch (const continuation_exception& e) {

    \ \ \ \ return e.continuation;

    }
  </cpp-code>

  \<#8FD9\>\<#4E2A\>\<#5B9E\>\<#73B0\>\<#53EF\>\<#4EE5\>\<#628A\>continuation\<#5F53\>\<#505A\><verbatim|return>\<#7528\>\<#FF0C\>\<#5373\>\<#63D0\>\<#524D\>\<#8FD4\>\<#56DE\>\<#3002\>\<#901A\>\<#8FC7\>\<#629B\>\<#51FA\>\<#5F02\>\<#5E38\>\<#FF0C\>\<#6355\>\<#83B7\>\<#5F02\>\<#5E38\>\<#FF0C\>\<#8BA9\>\<#63A7\>\<#5236\>\<#6D41\>\<#56DE\>\<#5230\>\<#8C03\>\<#7528\>\<#7684\>\<#5730\>\<#65B9\>\<#3002\>

  \<#5982\>\<#679C\>\<#4FDD\>\<#5B58\>\<#8FD9\>\<#4E2A\>continuation\<#FF0C\>\<#5728\>\<#522B\>\<#7684\>\<#5730\>\<#65B9\>\<#8C03\>\<#7528\>\<#FF0C\>\<#7531\>\<#4E8E\>\<#6CA1\>\<#6709\>\<#6355\>\<#83B7\>\<#5F02\>\<#5E38\>\<#FF0C\>\<#8FD9\>\<#4E2A\>\<#65F6\>\<#5019\>\<#FF0C\>\<#7A0B\>\<#5E8F\>\<#76F4\>\<#63A5\>\<#5D29\>\<#6E83\>\<#3002\>

  \<#6BD4\>\<#5982\>\<#8FD9\>\<#4E2A\>\<#6D4B\>\<#8BD5\>\<#7528\>\<#4F8B\>

  <\cpp-code>
    (define c #f)

    (display

    \ \ (string-append "a" "c"

    \ \ \ \ (call/cc (lambda (return)

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (begin

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (set! c return)

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ "hhh"

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ )))

    \ \ \ \ \ \ \ \ "\\n"))

    \;

    (c "555")

    (c "666")

    (c "777")
  </cpp-code>

  \<#4F1A\>\<#629B\>\<#51FA\>\<#5F02\>\<#5E38\> <shell|libc++abi: terminating
  with uncaught exception of type pscm::continuation_exception>

  <subsubsection|\<#7B2C\>\<#4E8C\>\<#9636\>\<#6BB5\>
  \<#5C06\>\<#629B\>\<#5F02\>\<#5E38\>\<#6539\>\<#6210\>\<#4E00\>\<#4E2A\>\<#65B0\>\<#7684\>lambda\<#8C03\>\<#7528\>>

  \<#8FD9\>\<#4E2A\>\<#505A\>\<#6CD5\>\<#6709\>\<#70B9\>\<#50CF\>CPS\<#FF0C\>\<#4F46\>\<#662F\>\<#4E0D\>\<#5F7B\>\<#5E95\>\<#3002\>\<#90A3\>\<#4E2A\>\<#65F6\>\<#5019\>\<#4E5F\>\<#4E0D\>\<#77E5\>\<#9053\>CPS\<#662F\>\<#5565\>\<#3002\>

  \<#6BD4\>\<#65B9\>\<#8BF4\>\<#FF0C\>\<#4E0A\>\<#9762\>\<#7684\>\<#90A3\>\<#4E2A\>\<#6D4B\>\<#8BD5\>\<#7528\>\<#4F8B\>\<#FF0C\>\<#53EF\>\<#4EE5\>\<#5C06\>\<#629B\>\<#5F02\>\<#5E38\>\<#7684\>\<#5730\>\<#65B9\>\<#6539\>\<#6210\>\<#5982\>\<#4E0B\>\<#7684\>lambda

  <\scm-code>
    (lambda (x)

    \ \ (display\ 

    \ \ \ \ (string-append "a" "c" x "\\n")))
  </scm-code>

  \<#8FD9\>\<#4E2A\>\<#5B9E\>\<#73B0\>\<#7684\>\<#95EE\>\<#9898\>\<#662F\>\<#FF0C\>\<#6709\>\<#65F6\>\<#5019\>\<#6839\>\<#672C\>\<#4E0D\>\<#77E5\>\<#9053\>\<#600E\>\<#4E48\>\<#6539\>\<#6210\>lambda\<#FF0C\>\<#6216\>\<#8005\>\<#6539\>\<#51FA\>\<#6765\>\<#7684\>lambda\<#5C31\>\<#662F\>\<#9519\>\<#7684\>\<#3002\>

  \<#5F53\>\<#65F6\>\<#4FEE\>\<#6539\>lambda\<#7684\>\<#5B9E\>\<#73B0\>\<#5982\>\<#4E0B\>

  <\cpp-code>
    std::optional\<less\>Cell\<gtr\> change_code(Cell code, const Cell& cc) {

    \ \ \ \ if (!is_pair(code)) {

    \ \ \ \ \ \ \ \ return std::nullopt;

    \ \ \ \ }

    \ \ \ \ while (is_pair(code)) {

    \ \ \ \ \ \ \ \ if (car(code) == cc) {

    \ \ \ \ \ \ \ \ \ \ \ \ return code;

    \ \ \ \ \ \ \ \ }

    \ \ \ \ \ \ \ \ auto tmp = car(code);

    \ \ \ \ \ \ \ \ auto val = change_code(tmp, cc);

    \ \ \ \ \ \ \ \ if (val.has_value()) {

    \ \ \ \ \ \ \ \ \ \ \ \ return val;

    \ \ \ \ \ \ \ \ }

    \ \ \ \ \ \ \ \ code = cdr(code);

    \ \ \ \ }

    \ \ \ \ return std::nullopt;

    }

    // get code from context

    auto code = ...;

    /* cc is (call/cc (lambda (return)

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (begin

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (set! c return)

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ "hhh"

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ )))

    \;

    */

    auto new_code = change_code(code, cc);

    // construct new_proc with new_code

    auto new_proc = ...;
  </cpp-code>

  <subsubsection|\<#7B2C\>\<#4E09\>\<#9636\>\<#6BB5\>
  \<#4F7F\>\<#7528\>\<#6808\>\<#5E27\>>

  \<#8FD9\>\<#4E2A\>\<#505A\>\<#6CD5\>\<#5176\>\<#5B9E\>\<#91C7\>\<#7528\>\<#7684\>\<#662F\>Copying
  whole stacks\<#65B9\>\<#6CD5\>\<#FF0C\>\<#53EA\>\<#662F\>\<#6211\>\<#7684\>\<#6808\>\<#5E27\>\<#5B9E\>\<#73B0\>\<#662F\>\<#9519\>\<#7684\>\<#3002\>\<#4E3B\>\<#8981\>\<#539F\>\<#56E0\>\<#662F\>\<#6211\>\<#81EA\>\<#5DF1\>\<#90FD\>\<#6CA1\>\<#641E\>\<#6E05\>\<#695A\>\<#FF0C\>\<#6267\>\<#884C\>\<#65B9\>\<#6CD5\>\<#4F53\>\<#65F6\>\<#FF0C\>\<#6808\>\<#5E27\>\<#5230\>\<#5E95\>\<#8BE5\>\<#600E\>\<#4E48\>\<#6539\>\<#3002\>

  \<#5F53\>\<#65F6\>\<#7684\>\<#5B9E\>\<#73B0\>\<#662F\>\<#4E00\>\<#4E2A\><verbatim|Frame>\<#7C7B\>\<#5B58\>\<#653E\>\<#6267\>\<#884C\>\<#4E2D\>\<#7684\>\<#5E27\>

  <\cpp-code>
    class Frame {

    public:

    \ \ \ \ Frame(SymenvPtr env, Cell cell) : m_env(env), m_expr(cell) {}

    private:

    \ \ \ \ SymenvPtr m_env;

    \ \ \ \ Cell m_expr;

    \ \ \ \ std::vector\<less\>Cell\<gtr\> m_args_stack;

    };

    using FrameStack \ = std::vector\<less\>Frame\<gtr\>;
  </cpp-code>

  \<#4E00\>\<#4E2A\><verbatim|Continuation>\<#7C7B\>\<#5B58\>\<#653E\>\<#5F53\>\<#524D\>\<#7684\>continuation\<#FF0C\>\<#4E5F\>\<#5C31\>\<#662F\>\<#6574\>\<#4E2A\>\<#6808\>\<#5E27\>\<#3002\>

  <\cpp-code>
    class Continuation {

    public:

    \ \ \ \ explicit Continuation(FrameStack frames) :
    m_frames(std::move(frames)) {

    \ \ \ \ \ \ \ \ // the last frame is call/cc

    \ \ \ \ \ \ \ \ m_frames.pop_back();

    \ \ \ \ };

    private:

    \ \ \ \ FrameStack m_frames;

    };

    \;

    // using ContPtr \ \ \ \ = std::shared_ptr\<less\>Continuation\<gtr\>;

    auto cont = std::make_shared\<less\>ContPtr::element_type\<gtr\>(m_frames);

    auto f = eval(senv, cadr(cell));

    Cell expr = list(f, cont);

    return eval(senv, expr);
  </cpp-code>

  \<#540C\>\<#6837\>\<#4E5F\>\<#901A\>\<#8FC7\>\<#629B\>\<#5F02\>\<#5E38\>\<#7684\>\<#65B9\>\<#5F0F\>\<#FF0C\>\<#6539\>\<#53D8\>\<#89E3\>\<#91CA\>\<#5668\>\<#7684\>\<#63A7\>\<#5236\>\<#6D41\>\<#FF0C\>\<#7B80\>\<#5355\>\<#7C97\>\<#66B4\>\<#76F4\>\<#63A5\>\<#91CD\>\<#542F\>\<#89E3\>\<#91CA\>\<#5668\>\<#FF0C\>\<#91CD\>\<#65B0\>\<#52A0\>\<#8F7D\>\<#6574\>\<#4E2A\>\<#6808\>\<#5E27\>\<#3002\>

  \<#4E0D\>\<#8FC7\>\<#FF0C\>\<#76EE\>\<#524D\>\<#7684\>\<#5B9E\>\<#73B0\>\<#5206\>\<#4E0D\>\<#6E05\>\<#FF0C\>\<#8FD9\>\<#4E2A\>\<#65F6\>\<#5019\>\<#7684\>\<#6808\>\<#5E27\>\<#662F\>\<#5728\>\<#6C42\>\<#53C2\>\<#6570\>\<#FF0C\>\<#8FD8\>\<#662F\>\<#6267\>\<#884C\>\<#65B9\>\<#6CD5\>\<#4F53\>\<#3002\>\<#867D\>\<#7136\>\<#80FD\>\<#901A\>\<#8FC7\>\<#86EE\>\<#591A\>continuation\<#7684\>\<#6D4B\>\<#8BD5\>\<#7528\>\<#4F8B\>\<#FF0C\>\<#4F46\>\<#662F\>\<#4ECD\>\<#7136\>\<#5B58\>\<#5728\>\<#8BB8\>\<#591A\>\<#95EE\>\<#9898\>\<#FF0C\>\<#5BFC\>\<#81F4\>continuation\<#7684\>\<#5B9E\>\<#73B0\>\<#662F\>\<#6B8B\>\<#7F3A\>\<#7684\>\<#3002\>

  \<#4ECE\>\<#73B0\>\<#5728\>\<#770B\>\<#5F53\>\<#65F6\>\<#7684\>\<#5B9E\>\<#73B0\>\<#FF0C\>\<#6216\>\<#8BB8\>\<#8FD8\>\<#9700\>\<#8981\>\<#52A0\>\<#4E00\>\<#4E9B\>\<#6807\>\<#8BB0\>\<#FF0C\>\<#4EE5\>\<#4FBF\>\<#6062\>\<#590D\>\<#6808\>\<#5E27\>\<#540E\>\<#53EF\>\<#4EE5\>\<#51C6\>\<#786E\>\<#7684\>\<#89E3\>\<#91CA\>Scheme\<#8868\>\<#8FBE\>\<#5F0F\>\<#3002\>

  <subsubsection|\<#7B2C\>\<#56DB\>\<#9636\>\<#6BB5\>
  \<#5728\>\<#5BC4\>\<#5B58\>\<#673A\>\<#5668\>\<#4E0A\>\<#6267\>\<#884C\>\<#5B57\>\<#8282\>\<#7801\>>

  \<#5728\>\<#5B66\>\<#4E60\>SICP 5.4 The Explicit-Control
  Evaluator\<#65F6\>\<#FF0C\>\<#53D1\>\<#73B0\>\<#5B83\>\<#6709\>\<#4E00\>\<#4E2A\><verbatim|CONTIUE>\<#5BC4\>\<#5B58\>\<#5668\>\<#FF0C\>\<#521A\>\<#597D\>\<#5BF9\>\<#5E94\>\<#540E\>\<#9762\>\<#8981\>\<#6267\>\<#884C\>\<#7684\>\<#6307\>\<#4EE4\>\<#4F4D\>\<#7F6E\>\<#3002\>\<#8FD9\>\<#4E0D\>\<#5C31\>\<#662F\>\<#6211\>\<#4E00\>\<#76F4\>\<#60F3\>\<#8981\>\<#7684\>continuation\<#5417\>\<#FF1F\>\<#4E8E\>\<#662F\>\<#53C2\>\<#8003\>\<#FF08\>\<#7167\>\<#6284\>\<#FF09\>SICP
  5.5 Compilation\<#505A\>\<#4E86\>\<#4E00\>\<#4E2A\>\<#7F16\>\<#8BD1\>\<#5668\>\<#3002\>

  \<#5BF9\>\<#4E8E\><verbatim|call/cc>\<#8C03\>\<#7528\>\<#FF0C\>\<#4E13\>\<#95E8\>\<#589E\>\<#52A0\>\<#4E00\>\<#4E2A\><verbatim|CONT>\<#6307\>\<#4EE4\>\<#FF0C\>\<#751F\>\<#6210\>continuation\<#3002\>\<#8BE5\>\<#6307\>\<#4EE4\>\<#6267\>\<#884C\>\<#65F6\>\<#FF0C\>\<#5C06\>\<#5F53\>\<#524D\>\<#7684\>\<#6808\>\<#548C\>\<#5BC4\>\<#5B58\>\<#5668\>\<#90FD\>\<#4FDD\>\<#5B58\>\<#4E0B\>\<#6765\>\<#3002\>\<#6B64\>\<#65F6\>\<#7684\><verbatim|Contiuation>\<#7C7B\>\<#578B\>\<#957F\>\<#8FD9\>\<#6837\>

  <\cpp-code>
    class Continuation {

    public:

    \ \ \ \ Continuation(std::stack\<less\>Cell\<gtr\> stack,
    std::unordered_map\<less\>Register, Cell\<gtr\> reg)

    \ \ \ \ \ \ \ \ : m_stack(std::move(stack))

    \ \ \ \ \ \ \ \ , m_reg(std::move(reg)) {

    \ \ \ \ }

    \ \ \ \ 

    \ \ \ \ [[nodiscard]] const std::stack\<less\>Cell\<gtr\>& stack() const
    {

    \ \ \ \ \ \ \ \ return m_stack;

    \ \ \ \ }

    \;

    \ \ \ \ [[nodiscard]] const std::unordered_map\<less\>Register,
    Cell\<gtr\>& reg() const {

    \ \ \ \ \ \ \ \ return m_reg;

    \ \ \ \ }

    \;

    private:

    \ \ \ \ std::stack\<less\>Cell\<gtr\> m_stack;

    \ \ \ \ std::unordered_map\<less\>Register, Cell\<gtr\> m_reg;

    };
  </cpp-code>

  \<#5F53\>\<#8C03\>\<#7528\>continuation\<#65F6\>\<#FF0C\>\<#4ECE\><verbatim|Continuation>\<#7C7B\>\<#5BF9\>\<#8C61\>\<#4E2D\>\<#6062\>\<#590D\>\<#6808\>\<#548C\>\<#5BC4\>\<#5B58\>\<#5668\>\<#3002\>

  <\cpp-code>
    // m is the virtual machine

    m.stack = cont-\<gtr\>stack();

    // val is the arg of calling continuation

    auto val = m.reg[Register::VAL];

    m.reg = cont-\<gtr\>reg();

    m.reg[Register::VAL] = val;

    // goto new position of instruction

    auto new_pos = m.reg.at(Register::CONTINUE);
  </cpp-code>

  \<#5BF9\>\<#4E8E\><verbatim|call/cc>\<#7684\>\<#7F16\>\<#8BD1\>\<#FF0C\>\<#4E5F\>\<#7ECF\>\<#5386\>\<#51E0\>\<#756A\>\<#53D8\>\<#5316\>

  \<#7B2C\>\<#4E00\>\<#7248\>\<#7F16\>\<#8BD1\>\<#51FA\>\<#6765\>\<#7684\>\<#6307\>\<#4EE4\>\<#5982\>\<#4E0B\>

  <\scm-code>
    (assign continue (label after_call))

    (cont)

    (assign argl (op list) (reg val))

    ;;; ignore proc call instruction

    after-call
  </scm-code>

  \<#8FD9\>\<#4E9B\>\<#6307\>\<#4EE4\>\<#5927\>\<#610F\>\<#5C31\>\<#662F\>\<#8C03\>\<#7528\>continuation\<#65F6\>\<#FF0C\>\<#7EE7\>\<#7EED\>\<#6267\>\<#884C\><verbatim|after-call>\<#540E\>\<#9762\>\<#7684\>\<#6307\>\<#4EE4\>\<#3002\>

  \<#8FD9\>\<#4E2A\>\<#5B9E\>\<#73B0\>\<#53EF\>\<#4EE5\>\<#628A\>continuation\<#5F53\>\<#8FD4\>\<#56DE\>\<#503C\>\<#FF0C\>\<#4F46\>\<#662F\>\<#4FDD\>\<#5B58\>continuation\<#540E\>\<#FF0C\>\<#8C03\>\<#7528\>\<#591A\>\<#6B21\>\<#FF0C\>\<#4F1A\>\<#9519\>\<#3002\>

  \<#6BD4\>\<#5982\>\<#8BF4\>\<#8FD9\>\<#4E2A\>\<#4F8B\>\<#5B50\>

  <\cpp-code>
    (define c #f)

    (display

    \ \ (string-append "a" "c"

    \ \ \ \ (call/cc (lambda (return)

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (begin

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ (set! c return)

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ "hhh"

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ )))

    \ \ \ \ \ \ \ \ "\\n"))

    \;

    (c "555")

    (c "666")

    (c "777")
  </cpp-code>

  <verbatim|(c "555")>\<#53EF\>\<#4EE5\>\<#6B63\>\<#5E38\>\<#8C03\>\<#7528\>\<#FF0C\>\<#6CA1\>\<#5565\>\<#95EE\>\<#9898\>\<#3002\>\<#4F46\>\<#662F\>\<#8C03\>\<#7528\><verbatim|(c
  "666")>\<#65F6\>\<#FF0C\>\<#4F1A\>\<#51FA\>\<#73B0\>\<#8C03\>\<#7528\><verbatim|(c
  "555")>\<#7684\>\<#60C5\>\<#51B5\>\<#3002\>\<#800C\>\<#8C03\>\<#7528\><verbatim|(c
  "777")>\<#65F6\>\<#FF0C\>\<#53C8\>\<#4F1A\>\<#5148\>\<#8C03\>\<#7528\><verbatim|(c
  "555")>\<#548C\><verbatim|(c "666")>\<#3002\>\<#51FA\>\<#73B0\>\<#8FD9\>\<#4E2A\>\<#95EE\>\<#9898\>\<#7684\>\<#539F\>\<#56E0\>\<#662F\>\<#7F16\>\<#8BD1\>\<#597D\>\<#7684\><verbatim|(c
  "666")>\<#7684\>\<#6307\>\<#4EE4\>\<#4F1A\>\<#653E\>\<#5728\><verbatim|(c
  "555")>\<#7684\>\<#6307\>\<#4EE4\>\<#540E\>\<#9762\>\<#FF0C\>\<#7F16\>\<#8BD1\>\<#7684\><verbatim|(c
  "777")>\<#7684\>\<#6307\>\<#4EE4\>\<#4F1A\>\<#653E\>\<#5728\><verbatim|(c
  "666")>\<#540E\>\<#9762\>\<#FF0C\>\<#800C\><verbatim|after-call>\<#540E\>\<#9762\>\<#63A5\>\<#7684\>\<#662F\><verbatim|(c
  "555")>\<#7684\>\<#6307\>\<#4EE4\>\<#3002\>

  \<#4E8E\>\<#662F\>\<#FF0C\>\<#6211\>\<#5BF9\>\<#6307\>\<#4EE4\>\<#7A0D\>\<#4F5C\>\<#4FEE\>\<#6539\>\<#FF0C\>\<#53BB\>\<#6389\>\<#4E86\>\<#6807\>\<#7B7E\><verbatim|after-call>\<#FF0C\>\<#5F97\>\<#5230\>\<#7B2C\>\<#4E8C\>\<#7248\>\<#3002\>

  <\scm-code>
    (cont)

    (assign argl (op list) (reg val))

    ;;; ignore proc call inst
  </scm-code>

  \<#5C06\>\<#51FD\>\<#6570\>\<#8C03\>\<#7528\>\<#7684\>\<#6307\>\<#4EE4\>\<#4E5F\>\<#5199\>\<#4E0A\>\<#FF0C\>\<#5C31\>\<#53D8\>\<#6210\>

  <\scm-code>
    (cont)

    (assgin argl (op list) (reg val))

    ;;; proc call inst

    (test (op primitive-procedure?) (reg proc))

    (branch primitive-branch)

    compiled-branch

    (assgin continue (label after-call))

    (assgin val (op compiled-procedure-entry) (reg proc))

    (goto val)

    primitive-branch

    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))

    after-call
  </scm-code>

  \<#8FD9\>\<#6837\>\<#4FEE\>\<#6539\>\<#540E\>\<#FF0C\>\<#4E0A\>\<#9762\>\<#7684\>\<#591A\>\<#6B21\>\<#8C03\>\<#7528\>\<#4E5F\>\<#6CA1\>\<#6709\>\<#95EE\>\<#9898\>\<#4E86\>\<#3002\>

  \<#5982\>\<#679C\>continuation\<#8FD9\>\<#6837\>\<#5B50\>\<#5C31\>\<#641E\>\<#5B9A\>\<#8BE5\>\<#591A\>\<#597D\>\<#FF0C\>\<#53EF\>\<#60DC\>\<#6CA1\>\<#6709\>\<#3002\>

  \<#770B\>\<#4E00\>\<#4E2A\>\<#591A\>\<#6B21\>\<#8C03\>\<#7528\><verbatim|call/cc>\<#7684\>\<#4F8B\>\<#5B50\>\<#3002\>

  <\scm-code>
    (define (f c) c)

    (let ((a (call/cc f)))

    \ \ (display "abc")

    \ \ (call/cc a))

    ;;; Output:

    ;;; abc#\<less\>none\<gtr\>

    ;;;

    ;;; However, Guile 1.8 Output:

    ;;; abcabc#\<less\>continuation 744 @ 7ffa06828a00\<gtr\>
  </scm-code>

  \<#8FD9\>\<#91CC\>\<#6709\>\<#4E24\>\<#4E2A\>\<#5730\>\<#65B9\>\<#4E0D\>\<#540C\>\<#FF0C\>\<#4E00\>\<#4E2A\>\<#662F\>abc\<#8F93\>\<#51FA\>\<#4E86\>\<#4E24\>\<#6B21\>\<#FF0C\>\<#53E6\>\<#5916\>\<#4E00\>\<#4E2A\>\<#6700\>\<#540E\>\<#8F93\>\<#51FA\>\<#7684\>\<#503C\>\<#662F\>continuation\<#3002\>

  \<#624B\>\<#52A8\>\<#6267\>\<#884C\>\<#4E00\>\<#4E0B\>

  \<#5F53\>\<#7B2C\>\<#4E00\>\<#4E2A\><verbatim|call/cc>\<#8C03\>\<#7528\><verbatim|(call/cc
  f)>\<#4E86\>\<#FF0C\>\<#53EF\>\<#4EE5\>\<#7B80\>\<#5355\>\<#5C06\>\<#4EE3\>\<#7801\>\<#8F6C\>\<#6362\>\<#4E3A\>

  <\scm-code>
    (f c1) ; c1 is cont, (lambda (x) (display "abc") (call/cc a))

    ; eval result: c1
  </scm-code>

  \<#7136\>\<#540E\>\<#6267\>\<#884C\><verbatim|(display
  "abc")>\<#FF0C\>\<#6253\>\<#5370\>\<#7B2C\>\<#4E00\>\<#4E2A\>abc\<#3002\>

  \<#63A5\>\<#7740\>\<#662F\>\<#7B2C\>\<#4E8C\>\<#4E2A\><verbatim|call/cc>\<#8C03\>\<#7528\><verbatim|(call/cc
  a)>\<#FF0C\>\<#4EE3\>\<#7801\>\<#53EF\>\<#4EE5\>\<#7B49\>\<#6548\>\<#8F6C\>\<#6362\>\<#4E3A\>\<#6267\>\<#884C\>

  <\scm-code>
    (c1 c2) ; c2 is cont, (lambda (x) )

    ; ==\<gtr\>

    (display "abc")\ 

    (call/cc a)
  </scm-code>

  \<#7136\>\<#540E\>\<#6267\>\<#884C\><verbatim|(display
  "abc")>\<#FF0C\>\<#6253\>\<#5370\>\<#7B2C\>\<#4E8C\>\<#4E2A\>abc\<#3002\>

  \<#63A5\>\<#7740\>\<#662F\>\<#7B2C\>\<#4E09\>\<#4E2A\><verbatim|call/cc>\<#8C03\>\<#7528\><verbatim|(call/cc
  a)>\ 

  \<#4E3A\>\<#5565\>\<#8FD9\>\<#91CC\>\<#662F\>\<#6253\>\<#5370\><verbatim|#\<less\>continuation
  744 @ 7ffa06828a00\<gtr\>>\<#FF1F\>

  \<#6211\>\<#59CB\>\<#7EC8\>\<#60F3\>\<#4E0D\>\<#901A\>\<#8FD9\>\<#4E2A\>\<#95EE\>\<#9898\>\<#3002\>\<#4E8E\>\<#662F\>\<#FF0C\>\<#6211\>\<#5F00\>\<#59CB\>\<#601D\>\<#8003\>\<#4E3A\>\<#5565\>\<#8981\>\<#6539\>\<#51FA\>\<#7B2C\>\<#4E8C\>\<#7248\>\<#FF1F\>

  \<#6539\>\<#7B2C\>\<#4E8C\>\<#7248\>\<#4E3B\>\<#8981\>\<#662F\>\<#56E0\>\<#4E3A\><verbatim|after-call>\<#6807\>\<#7B7E\>\<#540E\>\<#9762\>\<#8DDF\>\<#7684\>\<#4EE3\>\<#7801\>\<#4E0D\>\<#5E94\>\<#8BE5\>\<#7EE7\>\<#7EED\>\<#6267\>\<#884C\>

  \<#5982\>\<#679C\>\<#4E0D\>\<#5E94\>\<#8BE5\>\<#7EE7\>\<#7EED\>\<#6267\>\<#884C\>\<#7684\>\<#8BDD\>\<#FF0C\>\<#5E94\>\<#8BE5\><verbatim|goto>\<#5230\>\<#4E00\>\<#4E2A\>\<#65B0\>\<#7684\>\<#5730\>\<#65B9\>\<#53BB\>

  \<#8FD9\>\<#4E2A\>\<#65B0\>\<#7684\>\<#5730\>\<#65B9\>\<#672C\>\<#8D28\>\<#4E0A\>\<#662F\>\<#8FD8\>\<#4E00\>\<#6761\>\<#6307\>\<#4EE4\>\<#6240\>\<#5728\>\<#7684\>\<#4F4D\>\<#7F6E\>

  \<#90A3\>\<#4E48\>\<#4E0B\>\<#4E00\>\<#6761\>\<#6307\>\<#4EE4\>\<#5230\>\<#5E95\>\<#5728\>\<#54EA\>\<#91CC\>\<#FF1F\>

  \<#6211\>\<#4E8E\>\<#662F\>\<#62FF\>\<#7740\>\<#540C\>\<#6837\>\<#7684\>\<#4EE3\>\<#7801\>\<#5728\>\<#7B2C\>\<#4E00\>\<#7248\>\<#7684\>\<#6307\>\<#4EE4\>\<#4E0A\>\<#8DD1\>\<#FF0C\>\<#53D1\>\<#73B0\>\<#51FA\>\<#73B0\>\<#6B7B\>\<#5FAA\>\<#73AF\>

  <\scm-code>
    (define (f c) c)

    (let ((a (call/cc f)))

    \ \ (display "abc")

    \ \ (call/cc a))

    ;;; Output: (infinite loop)

    ;;; abcabcabcabc

    ;;;

    ;;; However, Guile 1.8 Output:

    ;;; abcabc#\<less\>continuation 744 @ 7ffa06828a00\<gtr\>
  </scm-code>

  \<#4E3A\>\<#5565\>Guile\<#53EA\>\<#662F\>\<#8F93\>\<#51FA\>\<#4E24\>\<#4E2A\>abc\<#548C\>1\<#4E2A\>cont\<#5C31\>\<#505C\>\<#4E86\>\<#5462\>\<#FF1F\>

  \<#800C\>\<#6211\>\<#7684\>\<#5374\>\<#662F\>\<#65E0\>\<#9650\>\<#5FAA\>\<#73AF\>\<#FF0C\>\<#5C31\>\<#50CF\>\<#6211\>\<#4E4B\>\<#524D\>\<#624B\>\<#52A8\>\<#6267\>\<#884C\>\<#4E00\>\<#6837\>\<#FF0C\>\<#8DD1\>\<#5230\>\<#7B2C\>\<#4E09\>\<#6B21\><verbatim|call/cc>\<#7684\>\<#5730\>\<#65B9\>\<#FF0C\>\<#61F5\>\<#4E86\>\<#3002\>

  \<#5728\>\<#7422\>\<#78E8\>\<#4E00\>\<#6BB5\>\<#65F6\>\<#95F4\>\<#540E\>\<#FF0C\>\<#6211\>\<#51B3\>\<#5B9A\>\<#6362\>\<#4E00\>\<#4E2A\>\<#601D\>\<#8DEF\>\<#3002\>\<#4E0D\>\<#518D\>\<#60F3\>\<#540E\>\<#9762\>\<#7684\>\<#6E90\>\<#7801\>\<#957F\>\<#4EC0\>\<#4E48\>\<#6837\>\<#5B50\>\<#FF0C\>\<#800C\>\<#662F\>\<#53BB\>\<#60F3\>\<#63A5\>\<#4E0B\>\<#6765\>\<#6307\>\<#4EE4\>\<#4F1A\>\<#8DF3\>\<#8F6C\>\<#5230\>\<#54EA\>\<#91CC\>\<#53BB\>\<#3002\>

  \<#6574\>\<#4E2A\>\<#4EE3\>\<#7801\>\<#53EF\>\<#4EE5\>\<#7B80\>\<#5355\>\<#8868\>\<#793A\>\<#4E3A\>

  <\scm-code>
    (cont) ; generate continuation

    ; call f

    after-call/cc-f

    ; display abc

    (cont) ; generate continuation

    ; call a

    after-call/cc-a
  </scm-code>

  <verbatim|after-call/cc-xxx>\<#8868\>\<#793A\>\<#8C03\>\<#7528\>continuation\<#540E\>\<#FF0C\>\<#6307\>\<#4EE4\>\<#4E3A\>\<#8DD1\>\<#5230\>\<#8FD9\>\<#4E2A\>\<#6807\>\<#7B7E\>\<#7684\>\<#4F4D\>\<#7F6E\>\<#3002\>

  \<#91CD\>\<#65B0\>\<#56DE\>\<#987E\>\<#4E00\>\<#4E0B\>\<#FF0C\>\<#6BCF\>\<#6B21\>\<#8C03\>\<#7528\>\<#51FD\>\<#6570\>\<#524D\>\<#FF0C\>\<#6211\>\<#4EEC\>\<#90FD\>\<#4F1A\>\<#4FDD\>\<#5B58\>\<#73B0\>\<#573A\>\<#FF0C\>\<#51FD\>\<#6570\>\<#8C03\>\<#7528\>\<#5B8C\>\<#FF0C\>\<#6062\>\<#590D\>\<#73B0\>\<#573A\>\<#3002\>

  \<#4FDD\>\<#5B58\>\<#73B0\>\<#573A\>\<#5373\><verbatim|(save continue)>

  \<#6062\>\<#590D\>\<#73B0\>\<#573A\>\<#5373\><verbatim|(restore continue)
  (goto continue)>

  \<#8C03\>\<#7528\>continuation\<#FF0C\>\<#4E5F\>\<#53EF\>\<#4EE5\>\<#770B\>\<#505A\>\<#662F\>\<#8C03\>\<#7528\>\<#4E00\>\<#4E2A\>\<#51FD\>\<#6570\>\<#3002\>

  \<#4F46\>\<#8FD9\>\<#4E2A\>\<#51FD\>\<#6570\>\<#FF0C\>\<#4ECE\>\<#73B0\>\<#5728\>\<#751F\>\<#6210\>\<#7684\>\<#6307\>\<#4EE4\>\<#770B\>\<#FF0C\>\<#662F\>\<#6CA1\>\<#6709\>\<#529E\>\<#6CD5\>\<#FF0C\>\<#5728\><verbatim|after-call/cc-f>\<#540E\>\<#6062\>\<#590D\>\<#73B0\>\<#573A\>\<#7684\>\<#3002\>

  \<#4E5F\>\<#5C31\>\<#662F\>\<#8868\>\<#73B0\>\<#4E3A\>\<#63A5\>\<#7740\>\<#6267\>\<#884C\>\<#540E\>\<#9762\>\<#7684\>\<#6307\>\<#4EE4\>\<#FF0C\>\<#4F46\>\<#662F\>\<#8FD9\>\<#4E2A\>\<#540E\>\<#9762\>\<#7684\>\<#6307\>\<#4EE4\>\<#4F4D\>\<#7F6E\>\<#4E0D\>\<#5BF9\>\<#3002\>

  \<#4FEE\>\<#590D\>\<#65B9\>\<#6CD5\>\<#5C31\>\<#662F\>\<#5728\><verbatim|after-call/cc-f>\<#540E\>\<#52A0\>\<#6062\>\<#590D\>\<#73B0\>\<#573A\>\<#7684\>\<#6307\>\<#4EE4\>\<#3002\>\<#8FD9\>\<#6837\>\<#5C31\>\<#6709\>\<#4E86\>\<#7B2C\>\<#4E09\>\<#7248\>\<#3002\>

  \<#7B2C\>\<#4E09\>\<#7248\>\<#7EC8\>\<#4E8E\>\<#53EF\>\<#4EE5\>\<#901A\>\<#8FC7\>\<#9634\>\<#9633\>\<#8C1C\>\<#9898\>\<#4E86\>

  <\scm-code>
    (let* ((yin

    \ \ \ \ \ \ \ \ ((lambda (foo)

    \ \ \ \ \ \ \ \ \ \ \ (display "@")

    \ \ \ \ \ \ \ \ \ \ \ foo)

    \ \ \ \ \ \ \ \ \ (call/cc (lambda (bar) bar))))

    \ \ \ \ \ \ \ (yang ((lambda (foo) (display "*") foo)

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ (call/cc (lambda (bar) bar)))))

    \ \ (yin yang))

    ;;; Output: (infinite loop)

    ;;; @*@**@***@****@*****@******@*******
  </scm-code>

  \<#5728\>\<#7B2C\>\<#4E8C\>\<#7248\>\<#7684\>\<#7ED3\>\<#5C3E\>\<#FF0C\>\<#867D\>\<#7136\>\<#5199\>\<#4E86\>\<#5F88\>\<#591A\>\<#FF0C\>\<#5176\>\<#5B9E\>\<#6700\>\<#5F00\>\<#59CB\>\<#6211\>\<#53EA\>\<#662F\>\<#6709\>\<#4E00\>\<#4E2A\>\<#611F\>\<#89C9\>\<#FF0C\>\<#4E5F\>\<#4E0D\>\<#77E5\>\<#9053\>\<#8BE5\>\<#600E\>\<#4E48\>\<#8DF3\>\<#3002\>\<#5728\>\<#770B\>\<#4E86\>\<#4E00\>\<#6BB5\>\<#65F6\>\<#95F4\>\<#4EE3\>\<#7801\>\<#540E\>\<#FF0C\>\<#53D1\>\<#73B0\>SICP\<#7248\>\<#672C\>\<#7684\>\<#7F16\>\<#8BD1\>\<#5668\>\<#FF0C\>\<#6BCF\>\<#4E2A\>\<#7F16\>\<#8BD1\>\<#51FD\>\<#6570\>\<#90FD\>\<#4F1A\>\<#4EE5\><code|end_with_linkage>\<#7ED3\>\<#5C3E\>\<#FF0C\>\<#800C\>\<#6211\>\<#7F16\>\<#8BD1\>continuation\<#65F6\>\<#5374\>\<#6CA1\>\<#6709\>\<#FF0C\>\<#4F1A\>\<#4E0D\>\<#4F1A\>\<#662F\>\<#8FD9\>\<#4E2A\>\<#539F\>\<#56E0\>\<#3002\>\<#6BD5\>\<#7ADF\><code|end_with_linkage>\<#8FD9\>\<#4E2A\>\<#51FD\>\<#6570\>\<#7684\>\<#4F5C\>\<#7528\>\<#5C31\>\<#662F\>\<#5904\>\<#7406\>\<#8DF3\>\<#8F6C\>\<#7684\>\<#3002\>\<#5728\>\<#52A0\>\<#4E86\>\<#8FD9\>\<#4E2A\>\<#51FD\>\<#6570\>\<#540E\>\<#FF0C\>\<#6211\>\<#60CA\>\<#5947\>\<#7684\>\<#53D1\>\<#73B0\>\<#FF0C\>\<#4E4B\>\<#524D\>\<#65E0\>\<#6CD5\>\<#901A\>\<#8FC7\>\<#7684\>\<#6D4B\>\<#8BD5\>\<#7528\>\<#4F8B\>\<#FF0C\>\<#5C45\>\<#7136\>\<#8FD0\>\<#884C\>\<#6210\>\<#679C\>\<#FF0C\>\<#6D4B\>\<#8BD5\>\<#901A\>\<#8FC7\>\<#3002\>\<#66F4\>\<#91CD\>\<#8981\>\<#7684\>\<#662F\>\<#FF0C\>\<#9634\>\<#9633\>\<#8C1C\>\<#9898\>\<#80FD\>\<#901A\>\<#8FC7\>\<#6D4B\>\<#8BD5\>\<#4E86\>\<#3002\>

  <subsection|\<#5B9E\>\<#73B0\>macro>

  <subsubsection|\<#4ECE\>\<#89E3\>\<#91CA\>\<#5668\>\<#6362\>\<#5230\>\<#7F16\>\<#8BD1\>\<#5668\>\<#FF0C\>\<#5B8F\>\<#5C55\>\<#5F00\>\<#9047\>\<#5230\>\<#7684\>\<#95EE\>\<#9898\>>

  \<#539F\>\<#4F5C\>\<#8005\>\<#5199\>\<#7684\>\<#89E3\>\<#91CA\>\<#5668\>\<#FF0C\>\<#5DF2\>\<#7ECF\>\<#5B9E\>\<#73B0\>\<#4E86\>lisp-style
  macro\<#3002\>\<#4F46\>\<#662F\>\<#FF0C\>\<#5F53\>\<#6211\>\<#4E3A\>\<#4E86\>\<#5B9E\>\<#73B0\>continuation\<#8FD9\>\<#4E2A\>\<#7279\>\<#6027\>\<#505A\>\<#4E86\>\<#7F16\>\<#8BD1\>\<#5668\>\<#540E\>\<#FF0C\>\<#539F\>\<#6709\>\<#7684\>macro\<#5DF2\>\<#7ECF\>\<#6CA1\>\<#6CD5\>\<#5B9E\>\<#73B0\>\<#5C55\>\<#5F00\>\<#4E86\>\<#3002\>\<#5176\>\<#4E3B\>\<#8981\>\<#539F\>\<#56E0\>\<#662F\>\<#539F\>\<#672C\>\<#7684\><verbatim|c++>\<#51FD\>\<#6570\>\<#4E2D\>\<#65E0\>\<#6CD5\>\<#8C03\>\<#7528\>\<#7F16\>\<#8BD1\>\<#597D\>\<#7684\>scheme\<#51FD\>\<#6570\>\<#3002\>\<#4EE5\><verbatim|let>\<#5B8F\>\<#4E3A\>\<#4F8B\>\<#FF0C\>\<#5148\>\<#4E0D\>\<#8003\>\<#8651\><verbatim|named-let>\<#3002\>

  <\scm-code>
    (define-macro (let bindings . body)

    \ \ `((lambda ,(map car bindings) . ,body) . ,@(map cadr bindings)))
  </scm-code>

  \<#8FD9\>\<#4E2A\><verbatim|let>\<#5B8F\>\<#4F1A\>\<#628A\>\<#539F\>\<#6709\>\<#7684\>\<#4EE3\>\<#7801\>\<#5C55\>\<#5F00\>\<#4E3A\>\<#4E00\>\<#4E2A\><verbatim|lambda>\<#53BB\>\<#8C03\>\<#7528\>\<#FF0C\>\<#6BD4\>\<#5982\>

  <\scm-code>
    (let ((a 1)

    \ \ \ \ \ \ (b 2))

    \ \ (+ a b) b)

    ;;; after expand

    ((lambda (a b)

    \ \ \ (+ a b)

    \ \ \ b)

    \ 1 2)
  </scm-code>

  \<#8FD9\>\<#4E2A\>\<#5B8F\>\<#7528\>\<#5230\>\<#4E86\>3\<#4E2A\>\<#7279\>\<#6027\>\<#FF08\>\<#51FD\>\<#6570\>\<#FF09\>\<#FF0C\>\<#5206\>\<#522B\>\<#662F\>

  <\itemize>
    <item>quasiquotation\<#FF0C\>\<#5373\>\<#7B26\>\<#53F7\><verbatim|`>

    <item>unquote\<#FF0C\>\<#5373\>\<#7B26\>\<#53F7\><verbatim|,>

    <item>unquote-splicing\<#FF0C\>\<#5373\>\<#7B26\>\<#53F7\><verbatim|,@>
  </itemize>

  Quasiquotation\<#53EF\>\<#4EE5\>\<#975E\>\<#5E38\>\<#65B9\>\<#4FBF\>\<#7684\>\<#7528\>\<#6A21\>\<#677F\>\<#53BB\>\<#6784\>\<#9020\>\<#4E00\>\<#4E2A\>\<#65B0\>\<#7684\>AST\<#FF0C\>\<#4E0D\>\<#7528\>\<#5199\>\<#7E41\>\<#5197\>\<#7684\><verbatim|cons>\<#3002\><verbatim|let>\<#5B8F\>\<#4E2D\>\<#7684\>\<#6A21\>\<#677F\>\<#4E3A\>

  <verbatim|((lambda ,(map car bindings) . ,body) . ,@(map cadr
  bindings))>\<#3002\>

  \<#5728\>\<#8FD9\>\<#4E2A\>\<#6A21\>\<#677F\>\<#4E2D\>\<#FF0C\><verbatim|,expr>\<#8868\>\<#793A\>\<#5BF9\>\<#8FD9\>\<#4E2A\><verbatim|expr>\<#8FDB\>\<#884C\>\<#6C42\>\<#503C\>\<#FF0C\>\<#5982\><verbatim|,(map
  car bindings)>\<#6C42\>\<#5F62\>\<#5F0F\>\<#53C2\>\<#6570\>\<#FF0C\><verbatim|,@expr>\<#8868\>\<#793A\>\<#6C42\>\<#503C\>\<#540E\>\<#8FD8\>\<#8981\>\<#53BB\>\<#6389\>\<#6700\>\<#5916\>\<#5C42\>\<#7684\>\<#62EC\>\<#53F7\>\<#FF0C\>\<#5982\><verbatim|,@(map
  cadr bindings)>\<#6C42\>\<#5B9E\>\<#53C2\>\<#3002\>

  \<#5728\>\<#6C42\>\<#503C\>\<#8FC7\>\<#7A0B\>\<#4E2D\>\<#FF0C\><verbatim|scheme>\<#7F16\>\<#8BD1\>\<#597D\>\<#7684\>\<#51FD\>\<#6570\>\<#8C03\>\<#7528\><verbatim|c++>\<#7684\>\<#51FD\>\<#6570\>\<#FF08\>\<#9664\>\<#5F00\><verbatim|call/cc>\<#8FD9\>\<#79CD\>\<#7279\>\<#6B8A\>\<#7684\>\<#FF09\>\<#8FD8\>\<#6BD4\>\<#8F83\>\<#597D\>\<#5F04\>\<#3002\>\<#5728\>\<#73B0\>\<#5728\>\<#7684\>PicoScheme\<#5B9E\>\<#73B0\>\<#4E2D\>\<#FF0C\>\<#591A\>\<#52A0\>\<#4E00\>\<#4E2A\><verbatim|Intern>\<#6620\>\<#5C04\>\<#4E00\>\<#4E0B\>\<#FF0C\>\<#95EE\>\<#9898\>\<#4E0D\>\<#5927\>\<#3002\>\<#4F46\>\<#662F\>\<#FF0C\>\<#5982\>\<#679C\>\<#4ECE\><verbatim|c++>\<#51FD\>\<#6570\>\<#4E2D\>\<#8C03\>\<#7528\><verbatim|scheme>\<#7F16\>\<#8BD1\>\<#597D\>\<#7684\>\<#51FD\>\<#6570\>\<#FF0C\>\<#5C31\>\<#4E0D\>\<#53EF\>\<#884C\>\<#4E86\>\<#3002\>\<#539F\>\<#56E0\>\<#5728\>\<#4E8E\>\<#FF0C\>\<#76EE\>\<#524D\>\<#57FA\>\<#4E8E\>SICP\<#7B2C\>5\<#7AE0\>\<#5B9E\>\<#73B0\>\<#7684\>\<#7F16\>\<#8BD1\>\<#5668\>\<#FF0C\>\<#7F16\>\<#8BD1\>\<#51FA\>\<#6765\>\<#7684\>scheme\<#51FD\>\<#6570\>\<#5B58\>\<#653E\>\<#5728\>\<#4E00\>\<#4E2A\>\<#53EB\><verbatim|CompiledProcedure>\<#7684\>\<#7C7B\>\<#4E2D\>\<#FF0C\>\<#4E3B\>\<#8981\>\<#5B58\>\<#653E\>\<#4E86\>\<#51FD\>\<#6570\>\<#7684\>\<#73AF\>\<#5883\>\<#548C\>\<#5165\>\<#53E3\>\<#3002\>\<#8FD9\>\<#4E2A\>\<#7C7B\>\<#7684\>\<#5B9A\>\<#4E49\>\<#5982\>\<#4E0B\>

  <\cpp-code>
    class CompiledProcedure {

    public:

    \ \ \ \ CompiledProcedure(std::shared_ptr\<less\>CompiledProcedureImpl\<gtr\>
    impl)

    \ \ \ \ \ \ \ \ : impl(std::move(impl)) {

    \ \ \ \ }

    \;

    \ \ \ \ bool operator!=(const CompiledProcedure& proc) const noexcept;

    \ \ \ \ bool operator==(const CompiledProcedure& proc) const noexcept;

    \ \ \ \ Cell env() const;

    \ \ \ \ Cell entry() const;

    \ \ \ \ // ignore hash function

    private:

    \ \ \ \ std::shared_ptr\<less\>CompiledProcedureImpl\<gtr\> impl;

    };

    \;

    struct CompiledProcedureImpl {

    \ \ \ \ CompiledProcedureImpl(MachineImpl& m, Label label,\ 

    \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ \ SymenvPtr senv, bool
    is_macro)

    \ \ \ \ \ \ \ \ : m(m)

    \ \ \ \ \ \ \ \ , label(label)

    \ \ \ \ \ \ \ \ , senv(senv)

    \ \ \ \ \ \ \ \ , is_macro(is_macro) {

    \ \ \ \ }

    \ \ \ \ // virtural machine to run the procedure

    \ \ \ \ MachineImpl& m;

    \ \ \ \ // procedure entry

    \ \ \ \ Label label;

    \ \ \ \ // procedure environment

    \ \ \ \ SymenvPtr senv;

    \;

    \ \ \ \ bool operator==(const CompiledProcedureImpl& rhs) const {

    \ \ \ \ \ \ \ \ return label == rhs.label && senv == rhs.senv;

    \ \ \ \ }

    \;

    \ \ \ \ bool operator!=(const CompiledProcedureImpl& rhs) const {

    \ \ \ \ \ \ \ \ return !(rhs == *this);

    \ \ \ \ }

    \;

    \ \ \ \ Int entry() const;

    };
  </cpp-code>

  <verbatim|CompiledProcedure>\<#662F\>\<#5BF9\><verbatim|CompiledProcedureImpl>\<#7684\>\<#5C01\>\<#88C5\>\<#FF0C\>\<#6211\>\<#8FD9\>\<#91CC\>\<#4E5F\>\<#6CA1\>\<#60F3\>\<#597D\>\<#600E\>\<#4E48\>\<#8D77\>\<#540D\>\<#5B57\>\<#FF0C\>\<#5148\>\<#8FD9\>\<#4E48\>\<#7528\>\<#7740\>\<#3002\><verbatim|CompiledProcedureImpl>\<#6709\>3\<#4E2A\>\<#5C5E\>\<#6027\>\<#FF0C\>\<#5206\>\<#522B\>\<#662F\>\<#8FD0\>\<#884C\>\<#4EE3\>\<#7801\>\<#7684\>\<#865A\>\<#62DF\>\<#673A\>\<#3001\>\<#51FD\>\<#6570\>\<#4EE3\>\<#7801\>\<#7684\>\<#5165\>\<#53E3\>\<#4EE5\>\<#53CA\>\<#51FD\>\<#6570\>\<#7684\>\<#73AF\>\<#5883\>\<#3002\>\<#865A\>\<#62DF\>\<#673A\>\<#5BF9\>\<#8C61\>\<#4E0D\>\<#662F\>\<#5FC5\>\<#8981\>\<#7684\>\<#FF0C\>\<#653E\>\<#7740\>\<#53EA\>\<#662F\>\<#4E3A\>\<#4E86\>\<#65B9\>\<#4FBF\>\<#8BA1\>\<#7B97\>\<#51FD\>\<#6570\>\<#5165\>\<#53E3\>\<#FF0C\>\<#540E\>\<#9762\>\<#53EF\>\<#4EE5\>\<#8003\>\<#8651\>\<#79FB\>\<#9664\>\<#3002\>Scheme\<#6E90\>\<#7801\>\<#7ECF\>\<#8FC7\>\<#7F16\>\<#8BD1\>\<#540E\>\<#4F1A\>\<#751F\>\<#6210\>\<#76F8\>\<#5BF9\>\<#5E94\>\<#7684\>\<#5B57\>\<#8282\>\<#7801\>\<#FF0C\>\<#6BCF\>\<#4E2A\>\<#51FD\>\<#6570\>\<#7684\>\<#51FD\>\<#6570\>\<#4F53\>\<#5BF9\>\<#5E94\>\<#7684\>\<#5B57\>\<#8282\>\<#7801\>\<#5728\>\<#7F16\>\<#8BD1\>\<#7684\>\<#65F6\>\<#5019\>\<#662F\>\<#6CA1\>\<#6CD5\>\<#786E\>\<#5B9A\>\<#4F4D\>\<#7F6E\>\<#FF08\>\<#5373\>\<#5730\>\<#5740\>\<#FF09\>\<#7684\>\<#FF0C\>\<#53EA\>\<#6709\>\<#628A\>\<#8FD9\>\<#4E9B\>\<#5B57\>\<#8282\>\<#7801\>\<#90FD\>\<#653E\>\<#5230\>\<#865A\>\<#62DF\>\<#673A\>\<#4E0A\>\<#8FD0\>\<#884C\>\<#65F6\>\<#FF08\>\<#7C7B\>\<#4F3C\>\<#52A0\>\<#8F7D\>\<#5230\>\<#5185\>\<#5B58\>\<#FF09\>\<#624D\>\<#80FD\>\<#786E\>\<#5B9A\>\<#3002\>

  \<#5728\>\<#539F\>\<#6765\>\<#7684\>\<#89E3\>\<#91CA\>\<#5668\>\<#4E2D\>\<#FF0C\>\<#6C42\>\<#503C\>\<#662F\>\<#901A\>\<#8FC7\><verbatim|eval>\<#51FD\>\<#6570\>\<#5B9E\>\<#73B0\>\<#7684\>\<#3002\>\<#76EE\>\<#524D\>\<#7F16\>\<#8BD1\>\<#5668\>\<#7684\>\<#5B9E\>\<#73B0\>\<#4E2D\>\<#FF0C\>\<#5F53\>\<#7F16\>\<#8BD1\>\<#5668\>\<#505A\>\<#5B8F\>\<#5F00\>\<#65F6\>\<#FF0C\>\<#4F1A\>\<#8C03\>\<#7528\>\<#89E3\>\<#91CA\>\<#5668\>\<#7684\><verbatim|eval>\<#51FD\>\<#6570\>\<#3002\>\<#800C\>\<#5728\>\<#89E3\>\<#91CA\>\<#5668\>\<#6C42\>\<#503C\>\<#65F6\>\<#FF0C\>\<#5982\>\<#679C\>\<#9700\>\<#8981\>\<#8FD0\>\<#884C\>\<#7F16\>\<#8BD1\>\<#5668\>\<#7F16\>\<#8BD1\>\<#51FA\>\<#6765\>\<#7684\>\<#51FD\>\<#6570\>\<#FF0C\>\<#89E3\>\<#91CA\>\<#5668\>\<#5C31\>\<#6302\>\<#4E86\>\<#3002\>\<#4EE5\>\<#6700\>\<#7B80\>\<#5355\>\<#7684\><verbatim|map>\<#4E3A\>\<#4F8B\>\<#FF0C\>\<#5728\><verbatim|c++>\<#4E2D\>\<#FF0C\><verbatim|map>\<#7684\>\<#5B9E\>\<#73B0\>\<#FF08\>\<#6682\>\<#65F6\>\<#53EA\>\<#8003\>\<#8651\>\<#53EA\>\<#6709\>\<#4E00\>\<#4E2A\>list\<#7684\>\<#60C5\>\<#51B5\>\<#FF09\>\<#5B9A\>\<#4E49\>\<#4E3A\>

  <\cpp-code>
    Cell list = args.at(1);

    Cell head = scm.cons(pscm::apply(scm, senv, proc, car(list)), nil);

    Cell tail = head;

    for (list = cdr(list); is_pair(list);\ 

    \ \ \ \ \ list = cdr(list), tail = cdr(tail)) {

    \ \ \ \ \ set_cdr(tail, scm.cons(pscm::apply(scm, senv, proc, car(list)),
    nil));

    \ \ \ \ }

    return head;
  </cpp-code>

  \<#8003\>\<#8651\>\<#8FD9\>\<#4E2A\>\<#7684\><verbatim|scheme>\<#4EE3\>\<#7801\>\<#FF1A\><scm|(map
  (lambda (x) x) '(1 2 3))>

  \<#8FD9\>\<#4E2A\><math|\<lambda\>> <verbatim|(lambda (x)
  x)>\<#8981\>\<#80FD\>\<#591F\>\<#5728\><verbatim|pscm::apply>\<#4E2D\>\<#6B63\>\<#5E38\>\<#6267\>\<#884C\>\<#FF0C\>\<#89E3\>\<#91CA\>\<#5668\>\<#624D\>\<#80FD\>\<#6B63\>\<#5E38\>\<#5DE5\>\<#4F5C\>\<#3002\>

  \<#5728\>\<#539F\>\<#6765\>\<#7684\>\<#89E3\>\<#91CA\>\<#5668\>\<#4E2D\>\<#FF0C\>\<#51FD\>\<#6570\>\<#7684\>\<#5B9A\>\<#4E49\>\<#7528\>\<#7C7B\><verbatim|Procedure>\<#5B9A\>\<#4E49\>\<#FF0C\>\<#5305\>\<#542B\>\<#51FD\>\<#6570\>\<#7684\>\<#73AF\>\<#5883\>\<#3001\>\<#884C\>\<#53C2\>\<#4EE5\>\<#53CA\>\<#51FD\>\<#6570\>\<#7684\>\<#4EE3\>\<#7801\>\<#FF0C\>\<#5176\>\<#7C7B\>\<#5B9A\>\<#4E49\>\<#5982\>\<#4E0B\>

  <\cpp-code>
    class Procedure {

    public:

    \ \ \ \ Procedure(const SymenvPtr& senv, const Cell& args, const Cell&
    code, bool is_macro = false);

    \ \ \ \ Cell senv() const noexcept;

    \ \ \ \ Cell args() const noexcept;

    \ \ \ \ Cell code() const noexcept;

    \ \ \ \ bool operator!=(const Procedure& proc) const noexcept;

    \ \ \ \ bool operator==(const Procedure& proc) const noexcept;

    \ \ \ \ std::pair\<less\>SymenvPtr, Cell\<gtr\> apply(Scheme& scm, const
    SymenvPtr& env, Cell args, bool is_list = true) const;

    \ \ \ \ // ignore hash function

    private:

    \ \ \ \ std::shared_ptr\<less\>Closure\<gtr\> impl;

    };

    \;

    struct Closure {

    \ \ \ \ Closure(SymenvPtr \ senv, const Cell& args, const Cell& code,
    bool is_macro)

    \ \ \ \ \ \ \ \ : senv{std::move( senv )}

    \ \ \ \ \ \ \ \ , args{ args }

    \ \ \ \ \ \ \ \ , code{ code }

    \ \ \ \ \ \ \ \ , entry{ none }

    \ \ \ \ \ \ \ \ , is_macro{ is_macro } {

    \ \ \ \ \ \ \ \ if (!is_unique_symbol_list(args) \|\| !is_pair(code))

    \ \ \ \ \ \ \ \ \ \ \ \ throw std::invalid_argument("invalid procedure
    definition");

    \ \ \ \ }

    \;

    \ \ \ \ bool operator!=(const Closure& impl) const noexcept {

    \ \ \ \ \ \ \ \ return senv != impl.senv \|\| args != impl.args \|\| code
    != impl.code \|\| is_macro != impl.is_macro;

    \ \ \ \ }

    \;

    \ \ \ \ SymenvPtr senv; //!\<less\> Symbol environment pointer.

    \ \ \ \ Cell args; \ \ \ \ \ //!\<less\> Formal parameter symbol list or
    single symbol.

    \ \ \ \ Cell code; \ \ \ \ \ //!\<less\> Lambda body expression list.

    };
  </cpp-code>

  <verbatim|Procedure::apply>\<#65B9\>\<#6CD5\>\<#5C31\>\<#662F\>\<#51FD\>\<#6570\>\<#8C03\>\<#7528\>\<#FF08\>\<#7684\>\<#51C6\>\<#5907\>\<#5DE5\>\<#4F5C\>\<#FF09\>\<#3002\>\<#5728\>\<#89E3\>\<#91CA\>\<#5668\>\<#7684\>\<#5B9E\>\<#73B0\>\<#4E2D\>\<#FF0C\>\<#8FD9\>\<#4E2A\>\<#51FD\>\<#6570\>\<#7528\>\<#6765\>\<#5C06\>\<#8BA1\>\<#7B97\>\<#5B9E\>\<#53C2\>\<#FF0C\>\<#5E76\>\<#8FD4\>\<#56DE\>\<#65B0\>\<#7684\>\<#73AF\>\<#5883\>\<#548C\>\<#51FD\>\<#6570\>\<#4EE3\>\<#7801\>\<#FF0C\>\<#6700\>\<#540E\>\<#5728\>\<#89E3\>\<#91CA\>\<#5668\>\<#5185\>\<#90E8\>\<#901A\>\<#8FC7\><verbatim|eval>\<#51FD\>\<#6570\>\<#5B8C\>\<#6210\>\<#51FD\>\<#6570\>\<#7684\>\<#8C03\>\<#7528\>\<#3002\>

  \<#603B\>\<#7684\>\<#6765\>\<#8BF4\>\<#FF0C\>\<#5C31\>\<#662F\>\<#89E3\>\<#91CA\>\<#5668\>\<#548C\>\<#7F16\>\<#8BD1\>\<#5668\>\<#4E24\>\<#8005\>\<#5BF9\>\<#51FD\>\<#6570\>\<#7684\>\<#8868\>\<#8FBE\>\<#4E0D\>\<#4E00\>\<#81F4\>\<#FF0C\>\<#5982\>\<#679C\>\<#80FD\>\<#591F\>\<#7EDF\>\<#4E00\>\<#8868\>\<#8FBE\>\<#FF0C\>\<#90A3\>\<#4E48\>\<#95EE\>\<#9898\>\<#53EF\>\<#4EE5\>\<#5F97\>\<#5230\>\<#89E3\>\<#51B3\>\<#3002\>

  <subsubsection|\<#7EDF\>\<#4E00\>\<#8868\>\<#8FBE\>\<#89E3\>\<#91CA\>\<#5668\>\<#548C\>\<#7F16\>\<#8BD1\>\<#5668\>\<#7684\>\<#51FD\>\<#6570\>>

  \<#9996\>\<#5148\>\<#9047\>\<#5230\>\<#7684\>\<#7B2C\>\<#4E00\>\<#4E2A\>\<#95EE\>\<#9898\>\<#5C31\>\<#662F\>\<#73B0\>\<#5728\>\<#5B9E\>\<#73B0\>\<#7684\>\<#201C\>\<#89E3\>\<#91CA\>\<#5668\>\<#201D\>\<#548C\>\<#201C\>\<#7F16\>\<#8BD1\>\<#5668\>\<#201D\>\<#6709\>\<#4EC0\>\<#4E48\>\<#533A\>\<#522B\>\<#FF1F\>

  \<#6216\>\<#8005\>\<#8BF4\>\<#662F\>\<#4E4B\>\<#524D\>\<#7684\>\<#201C\>\<#89E3\>\<#91CA\>\<#6267\>\<#884C\>\<#201D\>\<#548C\>\<#73B0\>\<#5728\>\<#7684\>\<#201C\>\<#7F16\>\<#8BD1\>\<#6267\>\<#884C\>\<#201D\>\<#6709\>\<#4EC0\>\<#4E48\>\<#533A\>\<#522B\>\<#FF1F\>

  \<#6211\>\<#89C9\>\<#5F97\>\<#73B0\>\<#5728\>\<#6700\>\<#5927\>\<#7684\>\<#533A\>\<#522B\>\<#5C31\>\<#662F\>\<#FF0C\>\<#4E4B\>\<#524D\>\<#7684\>\<#201C\>\<#89E3\>\<#91CA\>\<#6267\>\<#884C\>\<#201D\>\<#662F\>\<#57FA\>\<#4E8E\>AST\<#7684\>\<#FF0C\>\<#73B0\>\<#5728\>\<#7684\>\<#201C\>\<#7F16\>\<#8BD1\>\<#6267\>\<#884C\>\<#201D\>\<#662F\>\<#57FA\>\<#4E8E\>\<#5B57\>\<#8282\>\<#7801\>\<#7684\>\<#3002\>

  \<#201C\>\<#89E3\>\<#91CA\>\<#6267\>\<#884C\>\<#201D\>\<#9700\>\<#8981\>\<#4FDD\>\<#7559\>AST\<#FF0C\>\<#5373\>\<#539F\>\<#59CB\>\<#7684\>s\<#8868\>\<#8FBE\>\<#5F0F\>\<#4E0D\>\<#52A8\>\<#3002\>

  \<#201C\>\<#7F16\>\<#8BD1\>\<#6267\>\<#884C\>\<#201D\>\<#8981\>\<#5148\>\<#5C06\>s\<#8868\>\<#8FBE\>\<#5F0F\>\<#8F6C\>\<#6362\>\<#6210\>\<#4E00\>\<#4E2A\>\<#4E00\>\<#4E2A\>\<#7684\>\<#6307\>\<#4EE4\>\<#3002\>

  \<#600E\>\<#4E48\>\<#6837\>\<#8BA9\>\<#4E00\>\<#4E2A\>\<#51FD\>\<#6570\>\<#5373\>\<#53EF\>\<#4EE5\>\<#201C\>\<#89E3\>\<#91CA\>\<#6267\>\<#884C\>\<#201D\>\<#FF0C\>\<#6709\>\<#53EF\>\<#4EE5\>\<#201C\>\<#7F16\>\<#8BD1\>\<#6267\>\<#884C\>\<#201D\>\<#FF1F\>\<#76EE\>\<#524D\>\<#6765\>\<#770B\>\<#FF0C\>\<#540C\>\<#65F6\>\<#4FDD\>\<#7559\>AST\<#548C\>\<#5B57\>\<#8282\>\<#7801\>\<#53EF\>\<#80FD\>\<#4F1A\>\<#6BD4\>\<#8F83\>\<#597D\>\<#3002\>\<#800C\>\<#4E14\>\<#4E4B\>\<#524D\>\<#7684\>\<#4E00\>\<#4E9B\>\<#505A\>\<#6CD5\>\<#53EF\>\<#80FD\>\<#8FD8\>\<#9700\>\<#8981\>\<#53D8\>\<#52A8\>\<#3002\>

  \<#7B2C\>\<#4E00\>\<#4E2A\>\<#95EE\>\<#9898\>\<#FF1A\>\<#5982\>\<#4F55\>\<#8868\>\<#8FBE\>\<#5B57\>\<#8282\>\<#7801\>\<#4E2D\>\<#7684\>\<#6807\>\<#7B7E\>\<#FF1F\>

  AST\<#662F\>\<#6CA1\>\<#6709\>\<#6807\>\<#7B7E\>\<#8FD9\>\<#79CD\>\<#4E1C\>\<#897F\>\<#7684\>\<#3002\>\<#5B57\>\<#8282\>\<#7801\>\<#4E2D\>\<#7684\>\<#6807\>\<#7B7E\>\<#5728\>\<#6CA1\>\<#6709\>\<#52A0\>\<#8F7D\>\<#5230\>\<#865A\>\<#62DF\>\<#673A\>\<#4E2D\>\<#7684\>\<#65F6\>\<#5019\>\<#662F\>\<#4E0D\>\<#77E5\>\<#9053\>\<#4F4D\>\<#7F6E\>\<#7684\>\<#3002\>\<#662F\>\<#4E0D\>\<#662F\>\<#9700\>\<#8981\>\<#5728\>\<#7EE7\>\<#7EED\>\<#5728\><verbatim|Cell>\<#4E2D\>\<#52A0\>\<#4E00\>\<#4E2A\>\<#6807\>\<#7B7E\>\<#7684\>\<#503C\>\<#FF1F\>

  \<#7B2C\>\<#4E8C\>\<#4E2A\>\<#95EE\>\<#9898\>\<#FF1A\>\<#5B57\>\<#8282\>\<#7801\>\<#4F55\>\<#65F6\>\<#5E94\>\<#8BE5\>\<#52A0\>\<#8F7D\>\<#5230\>\<#865A\>\<#62DF\>\<#673A\>\<#4E2D\>\<#FF1F\>

  \<#76EE\>\<#524D\>\<#7684\>\<#505A\>\<#6CD5\>\<#662F\>\<#751F\>\<#6210\>\<#5B8C\>\<#540E\>\<#FF0C\>\<#5168\>\<#90E8\>\<#6254\>\<#5230\>\<#865A\>\<#62DF\>\<#673A\>\<#3002\>\<#7EDF\>\<#4E00\>\<#8868\>\<#8FBE\>\<#51FD\>\<#6570\>\<#540E\>\<#FF0C\>\<#662F\>\<#4E0D\>\<#662F\>\<#5E94\>\<#8BE5\>\<#505A\>\<#6309\>\<#9700\>\<#52A0\>\<#8F7D\>\<#5462\>\<#FF1F\>

  <section|\<#7406\>\<#89E3\>Scheme>

  <subsection|\<#4EC0\>\<#4E48\>\<#662F\>continuation>

  \<#5BF9\>\<#4E8E\>\<#8FD9\>\<#4E2A\>\<#6982\>\<#5FF5\>\<#FF0C\>\<#6211\>\<#5F00\>\<#59CB\>\<#4E5F\>\<#4E0D\>\<#77E5\>\<#9053\>\<#662F\>\<#4EC0\>\<#4E48\>\<#3002\>\<#5B83\>\<#7684\>\<#4E2D\>\<#6587\>\<#7FFB\>\<#8BD1\>\<#91C7\>\<#7528\>\<#7684\>\<#662F\>\<#201C\>\<#5EF6\>\<#7EED\>\<#201D\>\<#FF0C\>\<#90A3\>\<#201C\>\<#5EF6\>\<#7EED\>\<#201D\>\<#662F\>\<#4EC0\>\<#4E48\>\<#FF1F\>\<#4E0D\>\<#77E5\>\<#9053\>\<#3002\>

  \<#76F4\>\<#5230\>\<#6211\>\<#770B\>\<#5230\><hlink|\<#77E5\>\<#4E4E\>\<#7684\>\<#4E00\>\<#4E2A\>\<#56DE\>\<#7B54\>|https://www.zhihu.com/question/61222322/answer/193146161>

  <\quote-env>
    continuation\<#4FDD\>\<#5B58\>\<#4E86\>\<#5F53\>\<#524D\>\<#7A0B\>\<#5E8F\>\<#7684\>\<#4E0A\>\<#4E0B\>\<#6587\>\<#4EE5\>\<#53CA\>\<#73AF\>\<#5883\>\<#72B6\>\<#6001\>\<#FF0C\>\<#5982\>\<#679C\>\<#5B9E\>\<#73B0\>\<#8FC7\>\<#865A\>\<#62DF\>\<#673A\>\<#4E3A\>\<#57FA\>\<#7840\>\<#7684\>\<#89E3\>\<#91CA\>\<#5668\>\<#7684\>\<#8BDD\>\<#FF0C\>\<#53EF\>\<#4EE5\>\<#8BA4\>\<#4E3A\>continuation\<#4FDD\>\<#5B58\>\<#7684\>\<#662F\>\<#5F53\>\<#524D\>\<#8FD0\>\<#884C\>\<#65F6\>\<#7684\>\<#6808\>\<#548C\>\<#6240\>\<#6709\>\<#73AF\>\<#5883\>\<#53D8\>\<#91CF\>\<#4EE5\>\<#53CA\>\<#6267\>\<#884C\>call/cc\<#540E\>\<#7684\>\<#4E0B\>\<#4E00\>\<#6761\>\<#6307\>\<#4EE4\>\<#4F4D\>\<#7F6E\>\<#3002\>
  </quote-env>

  \<#4E0B\>\<#4E00\>\<#6761\>\<#6307\>\<#4EE4\>\<#8FD9\>\<#4E2A\>\<#6982\>\<#5FF5\>\<#FF0C\>\<#6211\>\<#7A81\>\<#7136\>\<#611F\>\<#89C9\>\<#81EA\>\<#5DF1\>\<#597D\>\<#50CF\>\<#61C2\>\<#4E86\>\<#3002\>\<#8FD9\>\<#4E2A\>\<#611F\>\<#89C9\>\<#4E0D\>\<#592A\>\<#597D\>\<#63CF\>\<#8FF0\>\<#FF0C\>\<#603B\>\<#4E4B\>\<#5C31\>\<#662F\>\<#90A3\>\<#4E2A\>\<#65F6\>\<#523B\>\<#5F88\>\<#60CA\>\<#559C\>\<#FF0C\>\<#539F\>\<#6765\>continuation\<#662F\>\<#8FD9\>\<#4E2A\>\<#6837\>\<#5B50\>\<#7684\>\<#3002\>

  \<#8FD8\>\<#6709\>\<#4E00\>\<#4E2A\>\<#662F\>\<#770B\>\<#5230\>SICP\<#8BB2\>\<#5230\>\<#5BC4\>\<#5B58\>\<#673A\>\<#5668\>\<#FF0C\>\<#4E5F\>\<#5C31\>\<#662F\>b\<#7AD9\>\<#4E0A\>Lec9a\<#FF0C\>Lec9b\<#65F6\>\<#FF0C\>\<#53D1\>\<#73B0\>\<#5B83\>\<#6709\>\<#4E00\>\<#4E2A\><verbatim|continue>\<#5BC4\>\<#5B58\>\<#5668\>\<#3002\>\<#5F53\>\<#65F6\>\<#7684\>\<#611F\>\<#89C9\>\<#5C31\>\<#662F\>\<#FF0C\>\<#8FD9\>\<#4E0D\>\<#5C31\>\<#662F\>continuation\<#5417\>\<#FF1F\>\<#8FD9\>\<#FF0C\>\<#8FD9\>\<#FF0C\>\<#8FD9\>......

  \<#63A5\>\<#4E0B\>\<#6765\>\<#53EF\>\<#4EE5\>\<#6D4B\>\<#8BD5\>\<#4E00\>\<#4E0B\>\<#81EA\>\<#5DF1\>\<#7684\>\<#6C34\>\<#5E73\>

  <\scm-code>
    ;;; test case 1

    (define (f c) c)

    (call/cc f)
  </scm-code>

  \<#5F88\>\<#7B80\>\<#5355\>\<#5427\>

  \<#518D\>\<#6765\>\<#4E00\>\<#4E2A\>

  <\scm-code>
    ;;; test case 2

    (call/cc (call/cc f))
  </scm-code>

  \<#770B\>\<#4E0A\>\<#53BB\>\<#FF0C\>\<#4E5F\>\<#5F88\>\<#7B80\>\<#5355\>\<#FF0C\>\<#5BF9\>\<#5427\>\<#FF1F\>

  \<#8001\>\<#5B9E\>\<#8BF4\>\<#FF0C\>\<#6211\>\<#73B0\>\<#5728\>\<#8FD8\>\<#662F\>\<#6CA1\>\<#7406\>\<#89E3\>\<#8FD9\>\<#4E2A\>\<#662F\>\<#600E\>\<#4E48\>\<#6267\>\<#884C\>\<#7684\>\<#3002\>\<#867D\>\<#7136\>\<#7F16\>\<#8BD1\>\<#51FA\>\<#6765\>\<#7684\>\<#6307\>\<#4EE4\>\<#53EF\>\<#4EE5\>\<#5F97\>\<#5230\>\<#6B63\>\<#786E\>\<#7684\>\<#7ED3\>\<#679C\>\<#3002\>

  \<#7EE7\>\<#7EED\>\<#7EE7\>\<#7EED\>

  <\scm-code>
    ;;; test case 3

    (let ((a (call/cc f)))

    \ \ (display "abc")

    \ \ (call/cc a))
  </scm-code>

  \<#6765\>\<#5427\>\<#FF0C\>\<#8F93\>\<#51FA\>\<#5565\>\<#FF1F\>

  \;

  <subsection|\<#4EC0\>\<#4E48\>\<#662F\>macro>

  \<#5B8F\>(macro)\<#8FD9\>\<#4E2A\>\<#4E1C\>\<#897F\>\<#FF0C\>\<#76EE\>\<#524D\>\<#5BF9\>\<#5B83\>\<#7684\>\<#5370\>\<#8C61\>\<#662F\>\<#FF0C\>\<#6709\>\<#4E00\>\<#4E2A\>\<#51FD\>\<#6570\>\<#FF0C\>\<#5B83\>\<#8F93\>\<#5165\>\<#4E00\>\<#4E2A\>AST\<#FF0C\>\<#8F93\>\<#51FA\>\<#4E00\>\<#4E2A\>AST\<#3002\>\<#4ECE\>\<#8FD9\>\<#4E2A\>\<#70B9\>\<#770B\>lisp-style
  macro\<#5C31\>\<#975E\>\<#5E38\>\<#597D\>\<#7406\>\<#89E3\>\<#3002\>

  \<#770B\>R5RS\<#6807\>\<#51C6\>\<#7684\>\<#65F6\>\<#5019\>\<#FF0C\>\<#53D1\>\<#73B0\>\<#91CC\>\<#9762\>\<#6709\>\<#4E00\>\<#4E2A\>\<#6A21\>\<#5F0F\>\<#5339\>\<#914D\>\<#FF0C\>\<#76EE\>\<#524D\>\<#8FD8\>\<#5F04\>\<#61C2\>\<#3002\>\<#5904\>\<#5728\>\<#80FD\>\<#770B\>\<#61C2\>\<#5927\>\<#6982\>\<#662F\>\<#4EC0\>\<#4E48\>\<#610F\>\<#601D\>\<#FF0C\>\<#4F46\>\<#662F\>\<#4E0D\>\<#77E5\>\<#9053\>\<#600E\>\<#4E48\>\<#5B9E\>\<#73B0\>\<#3002\>
</body>

<\initial>
  <\collection>
    <associate|page-height|auto>
    <associate|page-type|a4>
    <associate|page-width|auto>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|?>>
    <associate|auto-10|<tuple|3.2.3|?>>
    <associate|auto-11|<tuple|3.2.4|?>>
    <associate|auto-12|<tuple|3.3|?>>
    <associate|auto-13|<tuple|3.3.1|?>>
    <associate|auto-14|<tuple|3.3.2|?>>
    <associate|auto-15|<tuple|4|?>>
    <associate|auto-16|<tuple|4.1|?>>
    <associate|auto-17|<tuple|4.2|?>>
    <associate|auto-2|<tuple|2|?>>
    <associate|auto-3|<tuple|3|?>>
    <associate|auto-4|<tuple|3.1|?>>
    <associate|auto-5|<tuple|3.1.1|?>>
    <associate|auto-6|<tuple|3.1.2|?>>
    <associate|auto-7|<tuple|3.2|?>>
    <associate|auto-8|<tuple|3.2.1|?>>
    <associate|auto-9|<tuple|3.2.2|?>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|1<space|2spc>\<#5199\>\<#5728\>\<#524D\>\<#9762\>\<#7684\>\<#80E1\>\<#8A00\>\<#4E71\>\<#8BED\>>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-1><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|2<space|2spc>PicoScheme\<#5F00\>\<#53D1\>\<#8FC7\>\<#7A0B\>>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-2><vspace|0.5fn>

      <with|par-left|<quote|1tab>|2.1<space|2spc>\<#5B9E\>\<#73B0\>continuation
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-3>>

      <with|par-left|<quote|2tab>|2.1.1<space|2spc>\<#7B2C\>\<#4E00\>\<#9636\>\<#6BB5\>
      \<#539F\>\<#4F5C\>\<#8005\>arichel\<#5B9E\>\<#73B0\>\<#7684\>simple
      secape procedure <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-4>>

      <with|par-left|<quote|2tab>|2.1.2<space|2spc>\<#7B2C\>\<#4E8C\>\<#9636\>\<#6BB5\>
      \<#5C06\>\<#629B\>\<#5F02\>\<#5E38\>\<#6539\>\<#6210\>\<#4E00\>\<#4E2A\>\<#65B0\>\<#7684\>lambda\<#8C03\>\<#7528\>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-5>>

      <with|par-left|<quote|2tab>|2.1.3<space|2spc>\<#7B2C\>\<#4E09\>\<#9636\>\<#6BB5\>
      \<#4F7F\>\<#7528\>\<#6808\>\<#5E27\>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-6>>

      <with|par-left|<quote|2tab>|2.1.4<space|2spc>\<#7B2C\>\<#56DB\>\<#9636\>\<#6BB5\>
      \<#5728\>\<#5BC4\>\<#5B58\>\<#673A\>\<#5668\>\<#4E0A\>\<#6267\>\<#884C\>\<#5B57\>\<#8282\>\<#7801\>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-7>>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|3<space|2spc>\<#7406\>\<#89E3\>Scheme>
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-8><vspace|0.5fn>

      <with|par-left|<quote|1tab>|3.1<space|2spc>\<#4EC0\>\<#4E48\>\<#662F\>continuation
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-9>>

      <with|par-left|<quote|1tab>|3.2<space|2spc>\<#4EC0\>\<#4E48\>\<#662F\>macro
      <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-10>>
    </associate>
  </collection>
</auxiliary>