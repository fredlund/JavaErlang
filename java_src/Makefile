# Based on c_src.mk from erlang.mk by Loic Hoguin <essen@ninenines.eu>

CURDIR := .

JINTERFACE_PRIV_DIR ?= $(shell erl -noshell -eval "io:format(\"~s\", [code:priv_dir(jinterface)])." -s erlang halt)

JAVA_SRC_DIR = $(CURDIR)/java_src
JAVA_SRC_OUTPUT ?= $(CURDIR)/priv/JavaErlang.jar

# System type and C compiler/flags.

JAVAC ?= javac
JAR ?= jar

# Verbosity.

javac_verbose_0 = @echo " JAVAC     " $(?F);
javac_verbose = $(javac_verbose_$(V))

jar_verbose_0 = @echo " JAR    " $(@F);
jar_verbose = $(jar_verbose_$(V))

SOURCES := $(shell find $(JAVA_SRC_DIR) -type f \( -name "*.java" \))

COMPILE_JAVAC = $(javac_verbose) $(JAVAC) -g -deprecation -Werror \
	-sourcepath $(JAVA_SRC_DIR) \
	-classpath $(CURDIR)/priv/javassist.jar:$(JINTERFACE_PRIV_DIR)/OtpErlang.jar

$(JAVA_SRC_OUTPUT): $(SOURCES)
	@mkdir -p $(CURDIR)/priv/
	$(javac_verbose) $(COMPILE_JAVAC) -d $(JAVA_SRC_DIR) $(SOURCES)
	$(jar_verbose) $(JAR) cf $@ -C $(JAVA_SRC_DIR) javaErlang

clean:
	@rm -f `find $(JAVA_SRC_DIR) -name *.class` $(JAVA_SRC_OUTPUT)
