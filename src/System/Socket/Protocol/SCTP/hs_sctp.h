#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/sctp.h>

int hs_sctp_sendmsg(int sd, const void * msg, size_t len,
                    struct sockaddr *to, socklen_t tolen,
                    uint32_t ppid, uint32_t flags,
                    uint16_t stream_no, uint32_t timetolive,
                    uint32_t context, int *err);

int hs_sctp_recvmsg(int sd, void * msg, size_t len,
                    struct sockaddr * from, socklen_t * fromlen,
                    struct sctp_sndrcvinfo * sinfo, int * msg_flags,
                    int *err);
