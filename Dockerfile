FROM scratch

ARG REL_NAME
ARG REL_VSN=1
ARG ERTS_VSN

ENV BINDIR /erts-${ERTS_VSN}/bin
ENV BOOT /releases/${REL_VSN}/shelly_release
ENV CONFIG /releases/${REL_VSN}/sys.config
ENV ARGS_FILE /releases/${REL_VSN}/vm.args

ENV TZ=GMT

ENTRYPOINT exec ${BINDIR}/erlexec -boot_var ERTS_LIB_DIR /lib -boot ${BOOT} -noinput -config ${CONFIG} -args_file ${ARGS_FILE}

ADD _rel/shelly_release/ /
