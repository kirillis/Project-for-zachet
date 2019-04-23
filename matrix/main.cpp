#include <iostream>
#include <vector>
#include <string>
#include<time.h>
#include<cmath>

using namespace std;
//////////////////////////////////////////////////////////////////////////
const double eps = 1e-9;
	class Matrix{

	public:
		int m;
		int n;
		vector<vector<double> > vvi;
		Matrix() {}
		Matrix(int m, int n) {
			this->m = m;
			this->n = n;
			vvi.resize(m);
			for (int i = 0; i < m; i++) {
				vvi[i].resize(n);
			}
		}
		//Вывод матрицы(в дальнейшем и вектора) на экран
		void getPrint() {
			for (int i = 0; i < this->m; i++)
			{
				cout << "| ";
				for (int j = 0; j < this->n; j++)
					cout << "" << this->vvi[i][j] << " ";
				cout << "|";
				//cout << "" << endl;
				cout << "" << endl;
			}
			cout << endl;
		}
		//Считывание матрицы (построчно)
		void setValue() {
			cout << "Введите элементы" << endl;
			for (int i = 0; i < this->m; i++)
				for (int j = 0; j < this->n; j++)
					cin >> this->vvi[i][j];
			
		}
		// Заполнение матрицы случайными числами от 0 до 1 (использовалось для проверки других функций)
		void fillMatrix() {
			for (int i = 0; i < this->m; i++)
				for (int j = 0; j < this->n; j++)
					this->vvi[i][j] = rand() % 2;

		}
		void swapColumns(int i, int j) {
			for (int k = 0; k < this->m; k++)
				swap(this->vvi[k][i], this->vvi[k][j]);
		}
		void swapStrings(int i, int j) {
			for (int k = 0; k < this->n; k++)
				swap(this->vvi[i][k], this->vvi[j][k]);
		}
	};

	// Генерация единичной матрицы 
	Matrix E(Matrix& a) {
		Matrix e(a.m,a.m);
		for (int i = 0; i < e.m; i++)
			for (int j = 0; j < e.n; j++)
				e.vvi[i][j]=0;
		for (int i = 0; i < e.m; i++)
			e.vvi[i][i] = 1;
		return e;
	}
	Matrix E(int m) {
		Matrix e(m, m);
		for (int i = 0; i < m; i++)
			for (int j = 0; j < m; j++)
				e.vvi[i][j] = 0;
		for (int i = 0; i < m; i++)
			e.vvi[i][i] = 1;
		return e;
	}
	template <typename T>
	Matrix Sum(T& a, T& b) {
		if ((a.n != b.n) or (a.m != b.m))
		{
		}	//cout << "Некорректное выражение!" << endl;
		else
		{
			int n = a.n;
			int m = a.m;
			Matrix c(m, n);
			for (int i = 0; i < m; i++)
			{
				for (int j = 0; j < n; j++)
					c.vvi[i][j] = a.vvi[i][j] + b.vvi[i][j];
			}
			return c;
		}
	}
	template <typename T>
	Matrix Mult(T& a, T& b) {
		int m = a.m;
		int n = b.n;
		int f = b.m;
		Matrix c(m, n);
		for (int i = 0; i < m; i++) {
			for (int j = 0; j < n; j++)
			{
				c.vvi[i][j] = 0;
				for (int k = 0; k < f; k++)
					c.vvi[i][j] += a.vvi[i][k] * b.vvi[k][j];
			}
		}
		return c;
	}
	// Транспонирование матрицы
	Matrix Transp(Matrix& a) {
		Matrix c(a.n, a.m);
		for (int i = 0; i < a.m; i++)
			for (int j = 0; j < a.n; j++)
				c.vvi[j][i] = a.vvi[i][j];
		return c;
	}
	//Возврат матрицы без i-ой строки j-ого столбца
	Matrix GetMatr(Matrix& a, int i, int j) {
		int ki, kj, di, dj;
		Matrix p(a.m - 1, a.n - 1);
		di = 0;
		for (ki = 0; ki < a.m - 1; ki++) { // проверка индекса строки
			if (ki == i) di = 1;
			dj = 0;
			for (kj = 0; kj < a.m - 1; kj++) { // проверка индекса столбца
				if (kj == j) dj = 1;
				p.vvi[ki][kj] = a.vvi[ki + di][kj + dj];
			}
		}
		return p;
	}
	//Произведение матриц
	Matrix operator * (Matrix a, Matrix b) {
		int m = a.m;
		int n = b.n;
		int f = b.m;
		Matrix c(m, n);
		for (int i = 0; i < m; i++) {
			for (int j = 0; j < n; j++)
			{
				c.vvi[i][j] = 0;
				for (int k = 0; k < f; k++)
					c.vvi[i][j] += a.vvi[i][k] * b.vvi[k][j];
			}
		}
		return c;
	}
	// Подсчет детерминанта матрицы
	double det(Matrix a) {
		int i, j, k, n;
		double d;
		Matrix p(a.m, a.n);
		j = 0; d = 0;
		k = 1; //(-1) в степени i
		n = a.m - 1;
		if (a.m < 1) cout << "Определитель вычислить невозможно!";
		if (a.m == 1) {
			d = a.vvi[0][0];
			return d;
		}
		if (a.m == 2) {
			d = a.vvi[0][0] * a.vvi[1][1] - (a.vvi[1][0] * a.vvi[0][1]);
			return d;
		}
		if (a.m > 2) {
			for (i = 0; i < a.m; i++) {
				p = GetMatr(a, i, 0);
				//cout << a.vvi[i][j] << endl;
				if (a.vvi[i][0] == 0) continue;
				if(a.vvi[i][0] != 0) d += k * a.vvi[i][0] * det(p);
				k = -k;
			}
		}
		return d;
	}
	// Класс Вектор
	class Vector :public Matrix {
	public:
		Vector(int m) {
			this->m = m;
			this->n = 1;
			vvi.resize(m);
			for (int i = 0; i < m; i++) {
				vvi[i].resize(n);
			}
		}
	};
	// Преобраования вектора в матрицу и наоборот
	Matrix transV(Vector a) {
		Matrix x(a.m, 1);
		for (int i = 0; i < x.m; i++)
			x.vvi[i][0] = a.vvi[i][0];
		return x;
	}
	Vector transM(Matrix a) {
		Vector x(a.m);
		for (int i = 0; i < x.m; i++)
			x.vvi[i][0] = a.vvi[i][0];
		return x;
	}
	// Операторы
	template <typename T>
	T operator + (T a, T b) {
		int n = a.n;
		int m = a.m;
		for (int i = 0; i < m; i++)
		{
			for (int j = 0; j < n; j++)
				a.vvi[i][j] += b.vvi[i][j];
		}
		return a;

	}
	template <typename T>
	T operator - (T a, T b) {
		int n = a.n;
		int m = a.m;
		for (int i = 0; i < m; i++)
		{
			for (int j = 0; j < n; j++)
			a.vvi[i][j] -= b.vvi[i][j];
			}
			return a;
		}
	template <typename T>
	T operator * (double a, T x) {
		for (int i = 0; i < x.m; i++)
			for (int j = 0; j < x.n; j++)
				x.vvi[i][j] = x.vvi[i][j] * a;
		return x;

	}
	Vector operator * (Matrix a, Vector b) {
		Matrix f = a * transV(b);
		Vector x = transM(f);
		return x;
	}

	// Копируем К-ый столбец из матрицы
	Vector getVector(Matrix& a, int n) {
		Vector z(a.m);
		for (int i = 0; i < a.m; i++) {
			z.vvi[i][0] = a.vvi[i][n];
			//cout << "Типа скопировали " << z.vvi[i][0] << endl;
		}
		//getPrint(z);
		return z;
	}
	// Скалярное произведение
	double dotProduct(Vector a, Vector b) {// Скалярное произведение
		double d = 0;
		for (int i = 0; i < a.m; i++)
			d += a.vvi[i][0] * b.vvi[i][0];
		return d;
	}
	// Приписывание к матрице вектора
	Matrix addVector(Matrix& a, Vector& b) {
		a.n = a.n + 1;
		for (int i = 0; i < a.m; i++)
			a.vvi[i].push_back(b.vvi[i][0]);
		return a;
	}
	// Замена К-ого столбца вектором
	void setColumns(Matrix& a, Vector& b, int k) {
		for (int i = 0; i < a.m; i++)
			a.vvi[i][k] = b.vvi[i][0];

	}
	// Приведение матрицы к диагональному виду
	Matrix makeDiag(Matrix& a) {
		int i, j, m = 0, n = 0;
		double k;
		Matrix c=a;
		for (i = 0; i < a.m; ++i)
		{
			// находим строку с максимальным первым элементом
			int iMax = i;
			for (j = i + 1; j < a.m; ++j)
				if (abs(c.vvi[j][i]) > abs(c.vvi[iMax][i]))
					iMax = j;
			if (abs(c.vvi[iMax][i]) < eps)
				continue;
			for (k = 0; k < a.n; ++k)
				swap(c.vvi[i][k], c.vvi[iMax][k]);
			//countSwaps = -countSwaps * (i != iMax ? 1 : -1);

			//  вычитаем текущую строку из всех остальных
			for (j = i + 1; j < a.m; ++j)
			{
				double q = -c.vvi[j][i] / c.vvi[i][i];
				for (k = a.n - 1; k >= i; --k)
					c.vvi[j][k] += q * c.vvi[i][k];
			}
		}
		//c.getPrint();
		for (int i = 0;i<c.m;i++)
			if (c.vvi[i][i] != 0)
			{
				
				double tmp = c.vvi[i][i];
				for (int j = 0; j < c.n; j++)
				{
					c.vvi[i][j] = c.vvi[i][j] / tmp;
					
				}
			}
		for (int i = 0; i < c.m; i++)
		{
			if (c.vvi[i][i] != 0)
			{
				double tmp;
				for (int j = 0;j<c.m;j++)
					if (i != j)
					{
						tmp = c.vvi[j][i] / c.vvi[i][i];
						for (int k = 0; k < c.n; k++)
							c.vvi[j][k] -= c.vvi[i][k] * tmp;
					}
			}
		}
		
		return c;

	}
	// Возврат обратной матрицы
	Matrix Invert(Matrix& a) {
		Matrix b(a.m, 2 * a.n);
		for (int i = 0; i < a.m; i++)
			for (int j = 0; j < a.n; j++)
				b.vvi[i][j] = a.vvi[i][j];
		for (int i = 0; i < a.m; i++)
			b.vvi[i][i + a.n] = 1;
		b = makeDiag(b);
		Matrix c(a.m, a.n);
		for (int i = 0; i < a.m; i++)
			for (int j = 0; j < a.n; j++)
				c.vvi[i][j] = b.vvi[i][j + a.n];
		return c;
	}
	// Операторы, равенства и неравенства
	template <typename T>
	bool operator == (T a, T b) {
		int k = 1;
		for (int i = 0; i < a.m; i++)
			for (int j = 0; j < a.n; j++)
				if (a.vvi[i][j] != b.vvi[i][j])
					k = 0;
		if ((a.m != b.m) or (a.n != b.n))
			k = 0;
		return k;
	}
	template <typename T>
	bool operator != (T a, T b) {
		int k = 1;
		for (int i = 0; i < a.m; i++)
			for (int j = 0; j < a.n; j++)
				if (a.vvi[i][j] == b.vvi[i][j])
					k = 0;
		return k;
	}
	// Определение, является ли матрица ортагональной
	bool isOrth(Matrix& a) {
		Matrix b = Transp(a);
		if (((a * b) == E(a)) == 1 )
			return true;
		else
			return false;
	}
	// Решение системы методом Гаусса
	Vector solveGauss(Matrix& a, Vector& f) {
		cout << "Решение системы с помощью метода Гаусса" << endl;
		a = addVector(a, f);
		a.getPrint();
		a = makeDiag(a);
		a.getPrint();
		Vector x = getVector(a,a.n-1);
		cout << "Solution:" << endl;
		return x;
	}
	// Определение, является ли матрица симметричной
	bool isSymetric(Matrix& a) {
		int k = 1;
		for (int i = 0; i < a.m - 1; i++)
			for (int j = 1; j < a.n; j++)
				if (a.vvi[i][j] != a.vvi[j][i])
					k = 0;
		if (a.m != a.n)
			k = 0;
		return k;
	}
	// Определение является ли матрица положительно определенной
	bool isPosDetermined(Matrix& a) {// Реализация критерия Сильвестра
		int k = 1;
		for (int i = 0; i < a.m; i++)
		{
			Matrix tmp(i + 1, i + 1);
			for (int k = 0; k < i + 1; k++)
				for (int q = 0; q < i + 1; q++)
					tmp.vvi[k][q] = a.vvi[k][q];
			if ((det(tmp) > 0) == 0)
				k = 0;
			

		}
		return k;
	}

	////////////////////////////////////////////////////////////////////////////
	//Правило Крамера
	Vector solveCramer(Matrix a, Vector b) {
		cout << "Решение системы с помощью правила Крамера(абсолютно неэффективно для большх систем)" << endl;
		cout << "" << endl;
		Vector res(a.m);
		Vector r(a.m);
		double Delta = det(a);
		if (Delta != 0)
			for (int i = 0; i < a.n; i++)
			{
				r = getVector(a, i);
				setColumns(a, b, i);
				//cout << "a" << endl;
				double delta = det(a);
				cout << "delta" << delta << endl;
				res.vvi[i][0] = delta / Delta;
				for (int j = 0; j < a.m; j++)
					a.vvi[j][i] = r.vvi[j][0];
				//cout << res.vvi[i][0] << endl;
			}
		cout << "Solution is" << endl;
		return res;

	}
	////////////////////////////////////////////////////////////////////////////
	// Метод сопряженных градиентов (Работает только для положительно определенных матриц)
	Vector solveCG(Matrix a, Vector b) {
		cout << "Решение системы методом сопряженных градиентов" << endl;
		if (isPosDetermined(a) == 1)
		{
			Vector x(a.m);
			int i = 0;
			double alpha = 0;
			double betha = 0;
			for (int i = 0; i < x.m; i++)
				x.vvi[i][0] = 2;
			Vector r = b - a * x;
			Vector tmp(r.m);
			Vector z = r;
			while ((a*x != b)) {
				//начало i-ой итерации

				alpha = dotProduct(r, r) / dotProduct(a*z, z);
				x = x + alpha * z;
				tmp = r;
				r = r - alpha * (a*z);
				betha = alpha = dotProduct(r, r) / dotProduct(tmp, tmp);
				z = r + betha * z;
				i++;
				//x.getPrint();
			}
			for (int j = 0; j < x.m; j++)
				if (abs(x.vvi[j][0]) < 0.001)// с точностью до 8 знака
					x.vvi[j][0] = 0;
			return x;
		}
		else
			cout<<"Данная система не может быть решена методом сопряженных градиентов(может но автор не смог завставить ее работать)"<<endl;
	}


	////////////////////////////////////////////////////////////////////////////

	int main()
	{
		unsigned int start_time = clock(); 
		setlocale(LC_ALL, "ru");
		Matrix A(4, 4);
		A.setValue();
		Vector b(A.m);
		b.setValue();
		A = solveGauss(A,b);
		A.getPrint();
		unsigned int end_time = clock(); // конечное время
		unsigned int search_time = end_time - start_time;
		system("pause");
		return 0;
	}