#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>

int minici_fd_open_read( const char * from )
{
	return open( from, O_RDONLY | O_CLOEXEC );
}

int minici_fd_create_write( const char * from, int fd_perms )
{
	struct stat st;
	mode_t mode = 0600;
	if( fstat( fd_perms, & st ) == 0 )
		mode = st.st_mode;

	return open( from, O_CREAT | O_WRONLY | O_TRUNC | O_CLOEXEC, mode );
}
