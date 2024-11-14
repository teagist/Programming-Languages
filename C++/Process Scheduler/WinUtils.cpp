#include "WinUtils.h"


#ifdef _WIN32
#include <windows.h>
void delayBoot(int bootSeconds)
{
	std::string clk[4] = { "booting...|",
					 "booting.../",
					 "booting...-",
					 "booting...\\" };

	time_t seconds1     = time(NULL);
	time_t seconds2     = time(NULL);
	bool   bootComplete = false;
	int    idx          = 0;

	while (!bootComplete)
	{
		if ((time(NULL) - seconds1) >= bootSeconds)
		{
			bootComplete = true;
		}
		if ((time(NULL) - seconds2) >= 1)
		{
			seconds2 = time(NULL);
			for (int i = 0; i < 8; ++i)
			{
				std::cout << "\r" << clk[idx++] << " (" << (bootSeconds + 1) -
					(time(NULL) - seconds1) << " secs.)   ";
				if (idx > 3)
				{
					idx = 0;
				}
				Sleep(125);
			}
		}
	}
	std::cout << std::endl;
}

extern void setTitlebar(std::string testcase)
{
	std::stringstream title;
	std::string build = "1.1.0";
	
	title << "Passing" << "- " << testcase << " (Build: " << build << ")";
	SetConsoleTitle(title.str().c_str());
}

void clearScr()
{
	system("cls");
}



#else
#include <unistd.h>
void delayBoot(int bootSeconds)
{
	std::string clk[4] = { "booting...|",
					 	   "booting.../",
					       "booting...-",
					       "booting...\\" };

	time_t seconds1     = time(NULL);
	time_t seconds2     = time(NULL);
	bool   bootComplete = false;
	int    idx          = 0;

	while (!bootComplete)
	{
		if ((time(NULL) - seconds1) >= bootSeconds)
		{
			bootComplete = true;
		}
		if ((time(NULL) - seconds2) >= 1)
		{
			seconds2 = time(NULL);
			for (int i = 0; i < 8; ++i)
			{
				std::cout << "\r" << clk[idx++] << " (" << (bootSeconds + 1) -
					(time(NULL) - seconds1) << " secs.)   ";
				if (idx > 3)
				{
					idx = 0;
				}
				// * 1000 by the miliseconds to convert to microseconds
				usleep(125000);
			}
		}
	}
	std::cout << std::endl;
}

void clearScr()
{
	system("clear");
}
#endif
